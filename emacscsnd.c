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
   printf("code snippet: %s \n", code_snippet);
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
  const char* result;
  result = csoundGetOutputName(csound); 
  if (result == 0)
    {return 0;} else
    {return env->make_string(env,result,strlen(result));}
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
  MYFLT *chn =  env->get_user_ptr (env, args[1]);
  const char* chn_name = copy_string(env,args[2]);
  int chn_type = env->extract_integer (env, args[3]);
  int result = csoundGetChannelPtr(csound, &chn, chn_name, chn_type);

  return env->make_integer(env,result);
}

static emacs_value csndGetControlChannel (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* chn_name = copy_string(env,args[1]);
  MYFLT result = csoundGetControlChannel(csound, chn_name, NULL);

  return env->make_float(env,result);
}

static emacs_value csndSetControlChannel (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* chn_name = copy_string(env,args[1]);
  MYFLT val = env->extract_float (env, args[2]);
  csoundSetControlChannel(csound, chn_name, val);
  return 0;
}



static emacs_value csndGetStringChannel (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* chn_name = copy_string(env,args[1]);
  int str_size = csoundGetChannelDatasize(csound, chn_name);
  char *out_string;
  out_string = (char *) malloc(str_size);

  csoundGetStringChannel(csound, chn_name, out_string);

  return env->make_string(env,out_string, strlen(out_string));
}

static emacs_value csndSetStringChannel (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* chn_name = copy_string(env,args[1]);
  char* string_val = copy_string(env,args[2]);

  csoundSetStringChannel(csound, chn_name, string_val);
  return 0;
}

/* TODO: csoundScoreEvent csoundScoreEventAbsolute */

static emacs_value csndInputMessage (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* message = copy_string(env,args[1]);

  csoundInputMessage(csound, message);
  return 0;
}

/* Score Handling */

static emacs_value csndReadScore (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* score = copy_string(env,args[1]);
  int result = csoundReadScore(csound, score);
  return env->make_integer(env,result);
}

static emacs_value csndGetScoreTime (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  double result = csoundGetScoreTime(csound);
  return env->make_float(env, result);
}

static emacs_value csndIsScorePending (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  double result = csoundIsScorePending(csound);
  return env->make_integer(env, result);
}

static emacs_value csndSetScorePending (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int pending = env->extract_integer (env, args[1]);
  csoundSetScorePending(csound, pending);
  return 0;
}

static emacs_value csndGetScoreOffsetSeconds (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  MYFLT result = csoundGetScoreOffsetSeconds(csound);
  return env->make_float(env, result);
}

static emacs_value csndSetScoreOffsetSeconds (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  MYFLT time = env->extract_float (env, args[1]);
  csoundSetScoreOffsetSeconds(csound, time);
  return 0;
}

static emacs_value csndRewindScore (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  csoundRewindScore(csound);
  return 0;
}


/* Message buffer */

static emacs_value csndCreateMessageBuffer (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int to_std_out = env->extract_integer (env, args[1]);
  csoundCreateMessageBuffer(csound, to_std_out);
  return 0;
}

static emacs_value csndDestroyMessageBuffer (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  csoundDestroyMessageBuffer(csound);
  return 0;
}

static emacs_value csndGetFirstMessage (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  const char* first_message = csoundGetFirstMessage(csound);
  /* printf("first message %s \n", first_message); */
  if (first_message == NULL)
    {
      return env->make_integer(env, -1);
	}
  else {
    return env->make_string(env,first_message, strlen(first_message));
  }
}

static emacs_value csndGetFirstMessageAttr (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int attribute_parameter = csoundGetFirstMessageAttr(csound);
  return env->make_integer(env, attribute_parameter);
}

static emacs_value csndGetMessageCnt (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int message_count = csoundGetMessageCnt(csound);
  return env->make_integer(env, message_count);
}

static emacs_value csndSetMessageLevel (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int message_level = env->extract_integer (env, args[1]);
  csoundSetMessageLevel(csound, message_level);
  return 0;
}


static emacs_value csndGetMessageLevel (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  int message_level = csoundGetMessageLevel(csound);
  return env->make_integer(env, message_level);
}

static emacs_value csndPopFirstMessage (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]); 
  csoundPopFirstMessage(csound);
  return 0;
}

uintptr_t performance_function(void* data) {
  CSOUND* csound = (CSOUND*) data;
  while (csoundPerformKsmps(csound) == 0) {
  }
  return 0;
}

static emacs_value csndAsyncPerform (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  void* thread;
  thread = csoundCreateThread(&performance_function, (void*)csound); 
  return 0;
  csoundJoinThread(thread);
}


void csoundMessageCall(CSOUND* csound, int attr, const char* format, va_list valist);

static char* tty_output_writer_name = "";

static emacs_value csndMessageTty (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  CSOUND *csound = env->get_user_ptr (env, args[0]);
  tty_output_writer_name = copy_string(env,args[1]);
  /* CSOUND *csound = env->get_user_ptr (env, args[0]); */
  csoundSetMessageCallback(csound, csoundMessageCall); 
  return 0;
}

void csoundMessageCall(CSOUND* csound, int attr, const char* format, va_list valist)
{  
  /* int fd = open("/dev/pts/0", O_WRONLY); */
  int fd = open(tty_output_writer_name, O_WRONLY);
  char buffer [1024];
  int strout = vsnprintf(buffer,1024,format,valist);
  write(fd, buffer, strout);
}


/* static emacs_value csndMYFLTArray (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) */
/* { */
/*   /\* CSOUND *csound = env->get_user_ptr (env, args[0]); *\/ */
/*   int array_length = env->extract_integer (env, args[0]); */
/*   /\* code from: cs_glue.cpp#L487 *\/ */
/*   MYFLT* p =  0; */
/*   void*  pp =  0; */
/*   if (array_length > 0) */
/*     pp = (void*) malloc((size_t) array_length * sizeof(MYFLT)); */
/*   if (pp) { */
/*     p = (MYFLT*) pp; */
/*     for (int i = 0; i < array_length; i++) */
/*       p[i] = (MYFLT) 0; */
/*   }  */
/*   return env->make_user_ptr (env, null_finalizer, p); */
/* } */

/* static emacs_value csndMYFLTSetValue (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) */
/* { */
/*   MYFLT *array = env->get_user_ptr (env, args[0]); */
/*   int index = env->extract_integer (env, args[1]); */
/*   float value = env->extract_float(env, args[2]); */
/*   array[index] = value; */
/*   printf("array: %f\n", array[0]); */
/*   return 0; */
/* } */

/* static emacs_value csndMYFLTGetValue (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) */
/* { */
/*   MYFLT *array = env->get_user_ptr (env, args[0]); */
/*   int index = env->extract_integer (env, args[1]); */
/*   return env->make_float(env, array[index]); */
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

static void bind_constant (emacs_env *env, const char *name, int constant)
{
  emacs_value Qset = env->intern (env, "set");
  emacs_value Qsym = env->intern (env, name);
  emacs_value Qint = env->make_integer (env, constant);
  emacs_value args[] = { Qsym, Qint};

  env->funcall (env, Qset, 2, args);
 
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

  /* CONSTANTS */
  bind_constant(env, "CSOUND_CONTROL_CHANNEL", 1);
  bind_constant(env, "CSOUND_AUDIO_CHANNEL", 2);
  bind_constant(env, "CSOUND_STRING_CHANNEL", 3);
  bind_constant(env, "CSOUND_PVS_CHANNEL", 4);
  bind_constant(env, "CSOUND_VAR_CHANNEL", 5);
  bind_constant(env, "CSOUND_CHANNEL_TYPE_MASK", 15);
  bind_constant(env, "CSOUND_INPUT_CHANNEL", 16);
  bind_constant(env, "CSOUND_OUTPUT_CHANNEL", 32);

  bind_constant(env, "CSOUND_CONTROL_CHANNEL_NO_HINTS", 0);
  bind_constant(env, "CSOUND_CONTROL_CHANNEL_INT", 1);
  bind_constant(env, "CSOUND_CONTROL_CHANNEL_LIN", 2);
  bind_constant(env, "CSOUND_CONTROL_CHANNEL_EXP", 3);
  
  bind_constant(env, "CSOUNDINIT_NO_SIGNAL_HANDLER", 1);
  bind_constant(env, "CSOUNDINIT_NO_ATEXIT", 2);
  
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

  /* Channels, Control and Events */
  emacs_value CsoundGetChannelPtr = env->make_function (env, 4,4, csndGetChannelPtr, "Stores a pointer to the specified channel of the bus in *p, creating the channel first if it does not exist yet.", NULL);
  emacs_value CsoundGetControlChannel = env->make_function (env, 2,2, csndGetControlChannel, "retrieves the value of control channel identified by name.", NULL);
  emacs_value CsoundSetControlChannel = env->make_function (env, 3,3, csndSetControlChannel, "sets the value of control channel identified by *name", NULL);
  emacs_value CsoundGetStringChannel = env->make_function (env, 2,2, csndGetStringChannel, "retrieves the string channel identified by name", NULL);
  emacs_value CsoundSetStringChannel = env->make_function (env, 3,3, csndSetStringChannel, "Sets the string channel", NULL);
  /* emacs_value CsoundScoreEvent = env->make_function (env, 3, 1024,   csndScoreEvent, "Send a new score event.", NULL); */
  emacs_value CsoundInputMessage = env->make_function (env, 2, 2,   csndInputMessage, "Input a string, used for line events.", NULL);

  bind_function (env, "csoundGetChannelPtr", CsoundGetChannelPtr);
  bind_function (env, "csoundGetControlChannel", CsoundGetControlChannel);
  bind_function (env, "csoundSetControlChannel", CsoundSetControlChannel);
  bind_function (env, "csoundGetStringChannel", CsoundGetStringChannel);
  bind_function (env, "csoundSetStringChannel", CsoundSetStringChannel);
  /* bind_function (env, "csoundScoreEvent", CsoundScoreEvent); */
  bind_function (env, "csoundInputMessage", CsoundInputMessage);

  /* Score Handling */
  emacs_value CsoundReadScore = env->make_function (env, 2, 2, csndReadScore, "Read, preprocess, and load a score from an ASCII string It can be called repeatedly, with the new score events being added to the currently scheduled ones.", NULL);
  emacs_value CsoundGetScoreTime = env->make_function (env, 1, 1, csndGetScoreTime, "Returns the current score time in seconds since the beginning of performance.", NULL);
  emacs_value CsoundIsScorePending = env->make_function (env, 1, 1, csndIsScorePending, "Sets whether Csound score events are performed or not, independently of real-time MIDI events", NULL);
  emacs_value CsoundSetScorePending = env->make_function (env, 2, 2, csndSetScorePending, "Sets whether Csound score events are performed or not (real-time events will continue to be performed).", NULL);
  emacs_value CsoundGetScoreOffsetSeconds = env->make_function (env, 1, 1, csndGetScoreOffsetSeconds, "Returns the score time beginning at which score events will actually immediately be performed.", NULL);
  emacs_value CsoundSetScoreOffsetSeconds = env->make_function (env, 2, 2, csndSetScoreOffsetSeconds, "Csound score events prior to the specified time are not performed, and performance begins immediately at the specified time (real-time events will continue to be performed as they are received).", NULL);
  emacs_value CsoundRewindScore = env->make_function (env, 1, 1, csndRewindScore, "Rewinds a compiled Csound score to the time specified with (csoundSetScoreOffsetSeconds).", NULL);  

  bind_function (env, "csoundReadScore", CsoundReadScore);
  bind_function (env, "csoundGetScoreTime", CsoundGetScoreTime);
  bind_function (env, "csoundIsScorePending", CsoundIsScorePending);
  bind_function (env, "csoundSetScorePending", CsoundSetScorePending);
  bind_function (env, "csoundGetScoreOffsetSeconds", CsoundGetScoreOffsetSeconds);
  bind_function (env, "csoundSetScoreOffsetSeconds", CsoundSetScoreOffsetSeconds);
  bind_function (env, "csoundRewindScore", CsoundRewindScore);

  /* Message buffer */
  emacs_value CsoundCreateMessageBuffer = env->make_function (env, 2, 2, csndCreateMessageBuffer, "Creates a buffer for storing messages printed by Csound.", NULL);
  emacs_value CsoundDestroyMessageBuffer = env->make_function (env, 1, 1, csndDestroyMessageBuffer, "Releases all memory used by the message buffer.", NULL);
  emacs_value CsoundGetFirstMessage = env->make_function (env, 1, 1, csndGetFirstMessage, "Returns the first message from the buffer.", NULL);
  emacs_value CsoundGetFirstMessageAttr = env->make_function (env, 1, 1, csndGetFirstMessageAttr, "Returns the attribute parameter (see msg_attr.h) of the first message in the buffer.", NULL);
  emacs_value CsoundGetMessageCnt = env->make_function (env, 1, 1, csndGetMessageCnt, "Returns the number of pending messages in the buffer.", NULL);
  emacs_value CsoundSetMessageLevel = env->make_function (env, 2, 2, csndSetMessageLevel, "Sets the Csound message level (from 0 to 231).", NULL);
  emacs_value CsoundGetMessageLevel = env->make_function (env, 1, 1, csndGetMessageLevel, "Returns the Csound message level (from 0 to 231).", NULL);
  emacs_value CsoundPopFirstMessage = env->make_function (env, 1, 1, csndPopFirstMessage, "Removes the first message from the buffer.", NULL);
  
  bind_function (env, "csoundCreateMessageBuffer", CsoundCreateMessageBuffer);
  bind_function (env, "csoundDestroyMessageBuffer", CsoundDestroyMessageBuffer);
  bind_function (env, "csoundGetFirstMessage", CsoundGetFirstMessage);
  bind_function (env, "csoundGetFirstMessageAttr", CsoundGetFirstMessageAttr);
  bind_function (env, "csoundGetMessageCnt", CsoundGetMessageCnt);
  bind_function (env, "csoundSetMessageLevel", CsoundSetMessageLevel);
  bind_function (env, "csoundGetMessageLevel", CsoundGetMessageLevel);
  bind_function (env, "csoundPopFirstMessage", CsoundPopFirstMessage);

  /* Other */
  emacs_value CsoundAsyncPerform = env->make_function (env, 1, 1, csndAsyncPerform, "Same as csoundPerform but with on a seperate thread.", NULL);
  emacs_value CsoundMessageTty = env->make_function (env, 2, 2, csndMessageTty, "Print to TTY device instead of std_out.", NULL);
  
  bind_function (env, "csoundAsyncPerform", CsoundAsyncPerform);
  bind_function (env, "csoundMessageTty", CsoundMessageTty);
  
  /* Other */
  /* emacs_value CsoundMYFLTArray = env->make_function (env, 1, 1, csndMYFLTArray, "Creates MYFLT array of a given size.", NULL); */
  /* emacs_value CsoundMYFLTSetValue = env->make_function (env, 3, 3, csndMYFLTSetValue, "Given MYFLT array, set a float value on given index.", NULL); */
  /* emacs_value CsoundMYFLTGetValue = env->make_function (env, 2, 2, csndMYFLTGetValue, "Return the value of MYFLT array on a given index.", NULL); */
  
  /* bind_function (env, "csoundMYFLTArray", CsoundMYFLTArray); */
  /* bind_function (env, "csoundMYFLTSetValue", CsoundMYFLTSetValue); */
  /* bind_function (env, "csoundMYFLTGetValue", CsoundMYFLTGetValue); */

  provide (env, "csnd");
  

  return 0;
}
