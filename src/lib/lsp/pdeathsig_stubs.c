#include <caml/mlvalues.h>

#ifdef __linux__
#include <signal.h>
#include <sys/prctl.h>
#endif

/* Ask the kernel to SIGKILL this process when its parent dies.
   Returns false where this is not available (non-Linux) or fails. */
value mlsem_set_pdeathsig(value unit)
{
  (void)unit;
#ifdef __linux__
  return Val_bool(prctl(PR_SET_PDEATHSIG, SIGKILL) == 0);
#else
  return Val_false;
#endif
}
