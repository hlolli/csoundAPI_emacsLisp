#include <stdio.h>
#include <emacs-module.h>
#include "csound.h"


int plugin_is_GPL_compatible;

static void null_finalizer (void *ptr) {}

static char* copy_string (emacs_env *env, emacs_value str)
{
  ptrdiff_t length = 0;
  env->copy_string_contents (env, str, NULL, &length);
  if (env->non_local_exit_check (env))
    return NULL;

  char *name = malloc (length);
  env->copy_string_contents (env, str, name, &length);
  if (env->non_local_exit_check (env))
    {
      free (name);
      return NULL;
    }

  return name;
}


static emacs_value csndCreate (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound;
  csound = csoundCreate(NULL);
  /* result = env->make_user_ptr (env, null_finalizer, csound); */
  return env->make_user_ptr (env, null_finalizer, csound);
}

static emacs_value csndDestroy (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  csoundDestroy(csound); 
  return NULL;
}


static emacs_value csndGetAPIVersion (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int api_version = csoundGetAPIVersion();
  return env->make_integer (env, api_version);
}


static emacs_value csndGetVersion (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int version = csoundGetVersion();
  return env->make_integer (env, version);
}

static emacs_value csndInitialize (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int flag = env->extract_integer (env, args[0]);
  int result = csoundInitialize(flag);
  return env->make_integer (env, result);
}


static emacs_value csndCleanup (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int result = csoundCleanup(csound);
  return env->make_integer(env,result);
}

static emacs_value csndCompile (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* xargs[2];
  xargs[0] = "csound";
  xargs[1] = copy_string(env,args[1]);
  int result = csoundCompile(csound,2,xargs);
  return env->make_integer(env,result);
}

static emacs_value csndCompileArgs (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* xargs[2];
  xargs[0] = "csound";
  xargs[1] = copy_string(env,args[1]);
  int result = csoundCompileArgs(csound,2,xargs);
  return env->make_integer(env,result);
}

static emacs_value csndCompileCsd (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  char* filename = copy_string(env,args[1]);
  int result = csoundCompileCsd(csound, filename);
  return env->make_integer(env,result);
}

static emacs_value csndCompileCsdText (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csd_string = copy_string(env,args[1]);
  int result = csoundCompileCsd(csound, csd_string);
  return env->make_integer(env,result);
}

static emacs_value csndCompileOrc (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* orc_string = copy_string(env,args[1]);
  int result = csoundCompileOrc(csound, orc_string);
  return env->make_integer(env,result);
}

static emacs_value csndEvalCode (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* code_snippet = copy_string(env,args[1]);
  MYFLT result = csoundEvalCode(csound, code_snippet);
  return env->make_integer(env,result);
}

static emacs_value csndPerform (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int result = csoundPerform(csound);
  return env->make_integer(env,result);
}

static emacs_value csndPerformBuffer (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int result = csoundPerformBuffer(csound);
  return env->make_integer(env,result);
}

static emacs_value csndPerformKsmps (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int result = csoundPerformKsmps(csound);
  return env->make_integer(env,result);
}



/* static bool get_global (emacs_env *env, emacs_value *valptr, const char *name) */
/* { */
/*   *valptr = env->intern (env, name); */
/*   if (env->non_local_exit_check (env)) */
/*     return false; */
/*   *valptr = env->make_global_ref (env, *valptr); */
/*   return !env->non_local_exit_check (env); */
/* } */

/* static emacs_value Fmymod_test (emacs_env *env, int nargs, emacs_value args[], void *data) */
/* { */
/*   return env->make_integer (env, 42); */
/* } */



static void bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  /* Set the function cell of the symbol named NAME to SFUN using
     the 'fset' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);

  /* Prepare the arguments array */
  emacs_value args[] = { Qsym, Sfun };

  /* Make the call (2 == nb of arguments) */
  env->funcall (env, Qfset, 2, args);
}


static void provide (emacs_env *env, const char *feature)
{
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}


int emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

  /* create a lambda (returns an emacs_value) */
  /* emacs_value fun = env->make_function (env, */
  /* 					0,            /\* min. number of arguments *\/ */
  /* 					0,    /\* max. number of arguments *\/ */
  /* 					csndCreate,  */
  /* 					/\* Fmymod_test,  /\\* actual function pointer *\\/ *\/ */
  /* 					"add docstring",        /\* docstring *\/ */
  /* 					NULL          /\* user pointer of your choice (data param in Fmymod_test) *\/ */
  /* 					); */

  /* INSTANTIATION */ 
  emacs_value CsoundCreate = env->make_function (env, 0,0, csndCreate, "Creates an instance of Csound.", NULL);
  emacs_value CsoundDestroy = env->make_function (env, 1,1, csndDestroy, "Destroys an instance of Csound.", NULL);
  emacs_value CsoundInitialize = env->make_function (env, 1,1, csndInitialize, "Initialise Csound library with specific flags.", NULL);
  emacs_value CsoundGetAPIVersion = env->make_function (env, 0,0, csndGetAPIVersion, "Returns the API version number times 100 (1.00 = 100).", NULL);
  emacs_value CsoundGetVersion = env->make_function (env, 0,0, csndGetVersion, "Returns the version number times 1000 (5.00.0 = 5000).", NULL);

  bind_function (env, "csoundCreate", CsoundCreate);
  bind_function (env, "csoundDestroy", CsoundDestroy);
  bind_function (env, "csoundInitialize", CsoundInitialize);
  bind_function (env, "csoundGetAPIVersion", CsoundGetAPIVersion);
  bind_function (env, "csoundGetVersion", CsoundGetVersion);


  /* PERFORMANCE */
  emacs_value CsoundCleanup = env->make_function (env, 1,1, csndCleanup, "Prints information about the end of a performance, and closes audio and MIDI devices.", NULL);
  emacs_value CsoundCompile = env->make_function (env, 2,2, csndCompile, "Compiles Csound input files (such as an orchestra and score, or CSD) as directed by the supplied command-line arguments, but does not perform them. Calls (csoundStart) internally. Can only be called again after reset.", NULL);
  emacs_value CsoundCompileArgs = env->make_function (env, 2,2, csndCompileArgs, "Read arguments, parse and compile an orchestra, read, process and load a score.", NULL);
  emacs_value CsoundCompileCsd = env->make_function (env, 2,2, csndCompileCsd, "Compiles a Csound input file (CSD, .csd file) which includes command-line arguments, but does not perform the file. This function behaves similarly to (csoundCompile)", NULL);
  emacs_value CsoundCompileCsdText = env->make_function (env, 2,2, csndCompileCsdText, "Compiles a Csound input file contained in a string of text, which includes command-line arguments, orchestra, score, etc., but does not perform the file.", NULL);
  emacs_value CsoundCompileOrc = env->make_function (env, 2,2, csndCompileOrc, "Parse, and compile the given orchestra from an ASCII string, also evaluating any global space code (i-time only) this can be called during performance to compile a new orchestra.", NULL);

  emacs_value CsoundEvalCode = env->make_function (env, 2,2, csndEvalCode, "Parse, and compile the given orchestra from an ASCII string, also evaluating any global space code (i-time only) this can be called during performance to compile a new orchestra.", NULL);

  emacs_value CsoundPerform = env->make_function (env, 1,1, csndPerform, "Senses input events and performs audio output until the end of score is reached (positive return value), an error occurs (negative return value), or performance is stopped by calling (csoundStop) from another thread (zero return value).", NULL);
  emacs_value CsoundPerformBuffer = env->make_function (env, 1,1, csndPerformBuffer, "Performs Csound, sensing real-time and score events and processing one buffer's worth (-b frames) of interleaved audio.", NULL);
  emacs_value CsoundPerformKsmps = env->make_function (env, 1,1, csndPerformKsmps, "Senses input events, and performs one control sample worth (ksmps) of audio output.", NULL);


  bind_function (env, "csoundCleanup", CsoundCleanup);
  bind_function (env, "csoundCompile", CsoundCompile);
  bind_function (env, "csoundCompileArgs", CsoundCompileArgs);
  bind_function (env, "csoundCompileCsd", CsoundCompileCsd);
  bind_function (env, "csoundCompileCsdText", CsoundCompileCsdText);
  bind_function (env, "csoundCompileOrc", CsoundCompileOrc);
  bind_function (env, "csoundEvalCode", CsoundEvalCode);
  bind_function (env, "csoundPerform", CsoundPerform);
  bind_function (env, "csoundPerformBuffer", CsoundPerformBuffer);
  bind_function (env, "csoundPerformKsmps", CsoundPerformKsmps);
  
  provide (env, "csnd");


  return 0;
}
