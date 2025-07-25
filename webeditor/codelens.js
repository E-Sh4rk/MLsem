function applyChangesToRange(startL, endL, changes) {
    for (let i = 0; i < changes.length; i++) {
        let c = changes[i];
        let start = c.rangeOffset;
        let end = start + c.rangeLength;
        let diff = c.text.length - c.rangeLength;
        if (startL >= end) {
            startL += diff;
            endL += diff;
        }
        else if (endL <= start) { }
        else {
            return null;
        }
    }
    return [startL, endL];
}

function rangeOfPositions(start, end) {
    return  {
                startLineNumber: start.lineNumber,
                startColumn: start.column,
                endLineNumber: end.lineNumber,
                endColumn: end.column
            };
}

function isDummyPos(pos) {
    if (pos !== undefined && pos !== null)
        return pos["startOffset"] < 0 || pos["endOffset"] < 0;
    else
        return true;
}

function fullErrorMessage(res) {
    if(res["descr"] === undefined || res["message"] == null)
        return res["message"];
    else {
        return res["message"] + ":\n" + res["descr"];
    }
}

let codelensemitter = new monaco.Emitter();
let typesinfo = [];
function updatePos(arr, changes) {
    if (isDummyPos(arr["pos"])) arr["pos"] = null;
    if (arr["pos"] !== null) {
        let startD = arr["pos"]["startOffset"];
        let endD = arr["pos"]["endOffset"];
        let rangeD = applyChangesToRange(startD, endD, changes);
        if (rangeD === null) {
            arr["pos"] = null;
        }
        else {
            arr["pos"]["startOffset"] = rangeD[0];
            arr["pos"]["endOffset"] = rangeD[1];
        }
    }
}
function applyChangesToCurCodeLens(changes) {
    for (let i = 0; i < typesinfo.length; i++) {
        // Pos
        updatePos(typesinfo[i], changes);
        if (typesinfo[i]["messages"]) {
            for (let j = 0; j < typesinfo[i]["messages"].length; j++) {
                updatePos(typesinfo[i]["messages"][j], changes);
            }
        }
        else typesinfo[i]["messages"] = null;
        // Def_pos
        let startL = typesinfo[i]["def_pos"]["startOffset"];
        let endL = typesinfo[i]["def_pos"]["endOffset"];
        let range = applyChangesToRange(startL, endL, changes);
        if (range === null) {
            typesinfo.splice(i,1);
            i--;
        }
        else {
            typesinfo[i]["def_pos"]["startOffset"] = range[0];
            typesinfo[i]["def_pos"]["endOffset"] = range[1];
        }
    }
}
function updateTypeInfo(model, types, changes) {
    typesinfo = types;
    applyChangesToCurCodeLens(changes);
    codelensemitter.fire();
    validateMarkers(model);
}
function clearTypeInfo(model) {
    updateTypeInfo(model, [], []);
}

function validateMarkers(model) {
    const markers = [];
    function addMarker(info, severity) {
        if (info["severity"]) {
            switch (info["severity"]) {
            case "error":
                severity = monaco.MarkerSeverity.Error;
                break;
            case "warning":
                severity = monaco.MarkerSeverity.Warning;
                break;
            case "notice":
                severity = monaco.MarkerSeverity.Info;
                break;
            case "message":
                severity = monaco.MarkerSeverity.Hint;
                break;
            default:
            }
        }
        if (info["pos"] !== null) {
            let start = model.getPositionAt(info["pos"]["startOffset"]);
            let end = model.getPositionAt(info["pos"]["endOffset"]);
            let range = rangeOfPositions(start, end);
            markers.push({
				message: fullErrorMessage(info),
				severity: severity,
				startLineNumber: range.startLineNumber,
				startColumn: range.startColumn,
				endLineNumber: range.endLineNumber,
				endColumn: range.endColumn,
			});
        }
    }
    typesinfo.forEach((info) => {
        if (!info["typeable"]) {
            addMarker(info, monaco.MarkerSeverity.Error);
        }
        if (info["messages"] !== null) {
            for (let j = 0; j < info["messages"].length; j++) {
                addMarker(info["messages"][j], monaco.MarkerSeverity.Hint);
            }
        }
    });
    monaco.editor.setModelMarkers(model, "owner", markers);
}

function getCodeLens(editor, model) {
    model.onDidChangeContent((e) => { applyChangesToCurCodeLens(e.changes); });
    const messageContribution = editor.getContribution('editor.contrib.messageController');
    let copyCmd = editor.addCommand(0, function(ctx, ...arguments) {navigator.clipboard.writeText(arguments[0])});
    let errDetails = editor.addCommand(0, function(ctx, ...arguments) {
        let msg = arguments[1];
        let pos = editor.getPosition();
        if (arguments[0] !== null) pos = model.getPositionAt(arguments[0]);
        messageContribution.showMessage(msg, pos);
    });
    return {
        onDidChange: codelensemitter.event,
        provideCodeLenses: function (model, token) {
            let lenses = typesinfo.map(info => {
                let start = model.getPositionAt(info["def_pos"]["startOffset"]);
                let end = model.getPositionAt(info["def_pos"]["endOffset"]);
                let range = rangeOfPositions(start, end);
                let name = info["name"];
                if (info["typeable"]) {
                    let tooltip = "Inferred in "+Math.round(info["time"])+"ms\nClick to copy the type";
                    let type = info["type"];
                    return {range: range, id: name, command: {id: copyCmd, title: type, arguments: [type], tooltip: tooltip}}
                }
                else {
                    let dPos = null;
                    if (info["pos"] !== null)
                        dPos = info["pos"]["startOffset"];
                    let tooltip = "Inferred in "+Math.round(info["time"])+"ms\nClick for more info";
                    let msg = "Untypeable: "+info["message"];
                    let descr = fullErrorMessage(info);
                    return {range: range, id: name, command: {id: errDetails, title: msg, arguments: [dPos,descr], tooltip: tooltip}}
                }
            });
            return {
                lenses: lenses,
                dispose: () => {}
            };
        },
        resolveCodeLens: function (model, codeLens, token) {
            return codeLens;
        }
    };
}