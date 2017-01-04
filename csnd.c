#include <stdio.h>
#include <stdint.h>
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

/* MISSING csoundCompileTree and csoundDeleteTree */

static emacs_value csndEvalCode (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* code_snippet = copy_string(env,args[1]);
  MYFLT result = csoundEvalCode(csound, code_snippet);
  return env->make_integer(env,result);
}

static emacs_value csndInitializeCscore (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* filepathIn  = copy_string(env,args[1]);
  const char* filepathOut = copy_string(env,args[2]);
  FILE *inputFile = fopen(filepathIn, "r");
  FILE *outputFile = fopen(filepathOut, "w");
  int result = csoundInitializeCscore(csound, inputFile, outputFile);
  return env->make_integer(env,result);
}

/* MISSING csoundParseOrc */

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
 
static emacs_value csndReset (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  csoundReset(csound);
  return 0;
}

static emacs_value csndStart (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int result = csoundStart(csound);
  return env->make_integer(env,result);
}

static emacs_value csndStop (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  csoundStop(csound);
  return 0;
}


static emacs_value csndGetSr (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  MYFLT result = csoundGetSr(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGetKr (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  MYFLT result = csoundGetKr(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGetKsmps (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  uint32_t result = csoundGetKsmps(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGetNchnls (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  uint32_t result = csoundGetNchnls(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGetNchnlsInput (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  uint32_t result = csoundGetNchnlsInput(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGet0dBFS (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  MYFLT result = csoundGet0dBFS(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGetCurrentTimeSamples (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int64_t result = csoundGetCurrentTimeSamples(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGetDebug (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int result = csoundGetDebug(csound);
  return env->make_integer(env,result);
}

static emacs_value csndGetSizeOfMYFLT (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int result = csoundGetSizeOfMYFLT();
  return env->make_integer(env,result);
}

static emacs_value csndSetOption (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* compiler_flag = copy_string(env,args[1]);
  int result = csoundSetOption(csound, compiler_flag);
  return env->make_integer(env,result);
}

/* MISSING csoundSetHostData and csoundGetHostData */

/* MISSING csoundSetParams and csoundGetParams */


static emacs_value csndGetOutputName (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* result = csoundGetOutputName(csound);
  return env->make_string(env,result,sizeof(result));
}

/* IGNORED csoundSetFileOpenCallback */

static emacs_value csndSetInput (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csnd_input = copy_string(env,args[1]);
  csoundSetInput(csound, csnd_input);
  return 0;
}

static emacs_value csndSetMIDIFileInput (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csnd_midifile_input = copy_string(env,args[1]);
  csoundSetInput(csound, csnd_midifile_input);
  return 0;
}

static emacs_value csndSetMIDIFileOutput (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csnd_midifile_output = copy_string(env,args[1]);
  csoundSetInput(csound, csnd_midifile_output);
  return 0;
}

static emacs_value csndSetMIDIInput (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csnd_setmidi_input = copy_string(env,args[1]);
  csoundSetInput(csound, csnd_setmidi_input);
  return 0;
}

static emacs_value csndSetMIDIOutput (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csnd_setmidi_output = copy_string(env,args[1]);
  csoundSetInput(csound, csnd_setmidi_output);
  return 0;
}

static emacs_value csndSetOutput (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csnd_filename = copy_string(env,args[1]);
  const char* csnd_filetype = copy_string(env,args[2]);
  const char* csnd_fileformat = copy_string(env,args[3]);
  csoundSetOutput(csound, csnd_filename, csnd_filetype, csnd_fileformat);
  return 0;
}

/* Channels, Control and Events */

static emacs_value csndGetChannelPtr (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* csnd_input = copy_string(env,args[1]);
  csoundSetInput(csound, csnd_input);
  return 0;
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

static void bind_constant (emacs_env *env, const char *name, emacs_value Sfun)
{

  emacs_value Qfset = env->intern (env, "defconst");
  emacs_value Qsym = env->intern (env, name);

  /* emacs_value args[] = { Qsym, Sfun }; */

  /* env->funcall (env, Qfset, 2, args); */
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
  emacs_value CsoundInitializeCscore = env->make_function (env, 3,3, csndInitializeCscore, "Prepares an instance of Csound for Cscore processing outside of running an orchestra (i.e. \"standalone Cscore\")", NULL);

  emacs_value CsoundPerform = env->make_function (env, 1,1, csndPerform, "Senses input events and performs audio output until the end of score is reached (positive return value), an error occurs (negative return value), or performance is stopped by calling (csoundStop) from another thread (zero return value).", NULL);
  emacs_value CsoundPerformBuffer = env->make_function (env, 1,1, csndPerformBuffer, "Performs Csound, sensing real-time and score events and processing one buffer's worth (-b frames) of interleaved audio.", NULL);
  emacs_value CsoundPerformKsmps = env->make_function (env, 1,1, csndPerformKsmps, "Senses input events, and performs one control sample worth (ksmps) of audio output.", NULL);
  emacs_value CsoundReset = env->make_function (env, 1,1, csndReset, "Resets all internal memory and state in preparation for a new performance.", NULL);
  emacs_value CsoundStart = env->make_function (env, 1,1, csndStart, "Prepares Csound for performance after compilation using one or more of the above functions.", NULL);
  emacs_value CsoundStop = env->make_function (env, 1,1, csndStop, "Prepares Csound for performance after compilation using one or more of the above functions.", NULL);

  bind_function (env, "csoundCleanup", CsoundCleanup);
  bind_function (env, "csoundCompile", CsoundCompile);
  bind_function (env, "csoundCompileArgs", CsoundCompileArgs);
  bind_function (env, "csoundCompileCsd", CsoundCompileCsd);
  bind_function (env, "csoundCompileCsdText", CsoundCompileCsdText);
  bind_function (env, "csoundCompileOrc", CsoundCompileOrc);
  bind_function (env, "csoundEvalCode", CsoundEvalCode);
  bind_function (env, "csoundInitializeCscore", CsoundInitializeCscore);
  bind_function (env, "csoundPerform", CsoundPerform);
  bind_function (env, "csoundPerformBuffer", CsoundPerformBuffer);
  bind_function (env, "csoundPerformKsmps", CsoundPerformKsmps);
  bind_function (env, "csoundReset", CsoundReset);
  bind_function (env, "csoundStart", CsoundStart);
  bind_function (env, "csoundStop", CsoundStop);


  /* ATTRIBUTES */
  emacs_value CsoundGetSr = env->make_function (env, 1,1, csndGetSr, "Returns the number of audio sample frames per second.", NULL);
  emacs_value CsoundGetKr = env->make_function (env, 1,1, csndGetKr, "Returns the number of control samples per second.", NULL);
  emacs_value CsoundGetKsmps = env->make_function (env, 1,1, csndGetKsmps, "Returns the number of audio sample frames per control sample.", NULL);
  emacs_value CsoundGetNchnls = env->make_function (env, 1,1, csndGetNchnls, "Returns the number of audio output channels.", NULL);
  emacs_value CsoundGetNchnlsInput = env->make_function (env, 1,1, csndGetNchnlsInput, "Returns the number of audio input channels.", NULL);
  emacs_value CsoundGet0dBFS = env->make_function (env, 1,1, csndGet0dBFS, "Returns the 0dBFS level of the spin/spout buffers.", NULL);
  emacs_value CsoundGetCurrentTimeSamples = env->make_function (env, 1,1, csndGetCurrentTimeSamples, "Return the current performance time in samples.", NULL);
  emacs_value CsoundGetDebug = env->make_function (env, 1,1, csndGetDebug, "Returns whether Csound is set to print debug messages sent through the DebugMsg() internal API function. Anything different to 0 means true.", NULL);
  emacs_value CsoundGetSizeOfMYFLT = env->make_function (env, 0,0, csndGetSizeOfMYFLT, "Returns the 0dBFS level of the spin/spout buffers.", NULL);
  emacs_value CsoundSetOption = env->make_function (env, 2,2, csndSetOption, "Set a single csound option (flag).", NULL);

    
  bind_function (env, "csoundGetSr", CsoundGetSr);
  bind_function (env, "csoundGetKr", CsoundGetKr);
  bind_function (env, "csoundGetKsmps", CsoundGetKsmps);
  bind_function (env, "csoundGetNchnls", CsoundGetNchnls);
  bind_function (env, "csoundGetNchnlsInput", CsoundGetNchnlsInput);
  bind_function (env, "csoundGet0dBFS", CsoundGet0dBFS);
  bind_function (env, "csoundGetCurrentTimeSamples", CsoundGetCurrentTimeSamples);
  bind_function (env, "csoundGetDebug", CsoundGetDebug);
  bind_function (env, "csoundGetSizeOfMYFLT", CsoundGetSizeOfMYFLT);
  bind_function (env, "csoundSetOption", CsoundSetOption);

  /* GENERAL INPUT/OUTPUT */
  emacs_value CsoundGetOutputName = env->make_function (env, 1,1, csndGetOutputName, "Returns the output audio output name (-o).", NULL);
  emacs_value CsoundSetInput = env->make_function (env, 2,2, csndSetInput, "Set input source.", NULL);
  emacs_value CsoundSetMIDIFileInput = env->make_function (env, 2,2, csndSetMIDIFileInput, "Set MIDI file input name.", NULL);
  emacs_value CsoundSetMIDIFileOutput = env->make_function (env, 2,2, csndSetMIDIFileOutput, "Set MIDI file output name.", NULL);
  emacs_value CsoundSetMIDIInput = env->make_function (env, 2,2, csndSetMIDIInput, "Set MIDI input device name/number.", NULL);
  emacs_value CsoundSetMIDIOutput = env->make_function (env, 2,2, csndSetMIDIOutput, "Set MIDI output device name/number.", NULL);
  emacs_value CsoundSetOutput = env->make_function (env, 4,4, csndSetOutput, "Set output destination, type and format type can be one of \"wav\",\"aiff\", \"au\",\"raw\", \"paf\", \"svx\", \"nist\", \"voc\", \"ircam\",\"w64\",\"mat4\", \"mat5\", \"pvf\",\"xi\", \"htk\",\"sds\",\"avr\",\"wavex\",\"sd2\", \"flac\", \"caf\",\"wve\",\"ogg\",\"mpc2k\",\"rf64\", or NULL (use default or realtime IO). Format can be one of \"alaw\", \"schar\", \"uchar\", \"float\", \"double\", \"long\", \"short\", \"ulaw\", \"24bit\", \"vorbis\", or NULL (use default or realtime IO). For RT audio, use device_id from CS_AUDIODEVICE for a given audio device.", NULL);

  
  bind_function (env, "csoundGetOutputName", CsoundGetOutputName);
  bind_function (env, "csoundSetInput", CsoundSetInput);
  bind_function (env, "csoundSetMIDIFileInput", CsoundSetMIDIFileInput);
  bind_function (env, "csoundSetMIDIFileOutput", CsoundSetMIDIFileOutput);
  bind_function (env, "csoundSetMIDIInput", CsoundSetMIDIInput);
  bind_function (env, "csoundSetMIDIOutput", CsoundSetMIDIOutput);
  bind_function (env, "csoundSetOutput", CsoundSetOutput);
  
  provide (env, "csnd");


  return 0;
}
