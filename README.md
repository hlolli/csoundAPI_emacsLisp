# csoundAPI_emacsLisp
EmacsLisp link to Csound's API via Emacs Modules.



## Instantiation
https://csound.github.io/docs/api/group__INSTANTIATION.html

| API function     | Status        | Parameters |  Comment |
| ---------------- |:-------------:|:---------- |:-------- |
| csoundInitialize | available     | int(flags) |          |
| csoundCreate     | available     |            |          |
| csoundDestroy	   | available     | csnd       |          |
| csoundGetAPIVersion | available  |            |          |
|csoundGetVersion  |   available    |            |          |


## Performance
https://csound.github.io/docs/api/group__PERFORMANCE.html

| API function      | Status        | Parameters |  Comment |
| ----------------- |:-------------:|:---------- |:-------- |
| csoundParseOrc    | missing  |               |           |
| csoundCompileTree | missing  |               |           |
| csoundDeleteTree  | missing  |               |           |
| csoundCompileOrc | available | csnd, str | |
| csoundEvalCode | available | csnd, str | |
| csoundInitializeCscore | available | csnd, str(input file), str(output file)| |
| csoundCompileArgs | available| csnd, str | |
| csoundStart   | available | csnd | |
| csoundCompile | available | csnd, str | | 
| csoundCompileCsd | available | csnd, str(filename)| |
| csoundCompileCsdText | available | csnd, str | |
| csoundPerform | available | csnd | |
| csoundPerformKsmps | available | csnd | |
| csoundPerformBuffer | available | csnd | |
| csoundStop   | available | csnd | |
| csoundCleanup | available | csnd | |
| csoundReset  | available | csnd | |

## Attributes
https://csound.github.io/docs/api/group__ATTRIBUTES.html

| API function      | Status        | Parameters |  Comment |
| ----------------- |:-------------:|:---------- |:-------- |
| csoundGetSr   | available | csnd | |
| csoundGetKr   | available | csnd | |
| csoundGetKsmps  | available | csnd | |
| csoundGetNchnls   | available | csnd | |
| csoundGetNchnlsInput   | available | csnd | |
| csoundGet0dBFS   | available | csnd | |
| csoundGetCurrentTimeSamples   | available | csnd | |
| csoundGetSizeOfMYFLT   | available |  | |
| csoundGetHostData | missing | | |
| csoundSetHostData | missing | | |
| csoundSetOption   | available | csnd, str | |
| csoundSetParams   | missing | | |
| csoundGetParams   | missing | | |
| csoundGetDebug  | available | csnd | |
| csoundSetDebug  |  missing | | |

## General Input/Output
https://csound.github.io/docs/api/group__FILEIO.html

| API function      | Status        | Parameters |  Comment |
| ----------------- |:-------------:|:---------- |:-------- |
| csoundGetOutputName|available    | csnd       | |
| csoundSetOutput   | available | csnd,str(filename),str(filetype),str(fileformat)||
|csoundSetInput      | available     | csnd,str (input name) | |
|csoundSetMIDIInput  |available   | csnd, str(midiinputname)| |
|csoundSetMIDIOutput |available   | csnd, str(midioutputname)| |
|csoundSetMIDIFileInput |available   | csnd, str(midifilename)| |
|csoundSetMIDIFileOutput |available   | csnd, str(midifilename)| |
|csoundSetFileOpenCallback | missing | | | 

## Channels, Control and Events
https://csound.github.io/docs/api/group__CONTROLEVENTS.html

| API function      | Status        | Parameters |  Comment |
| ----------------- |:-------------:|:---------- |:-------- |
| csoundGetChannelPtr | available | csnd,chn,str(name),int(type)|MYFLT unimplemented|
| csoundListChannels | missing | | |
| csoundDeleteChannelList | missing | | |
| csoundSetControlChannelHints | missing | | |
| csoundGetControlChannelHints | missing | | |
| csoundGetChannelLock | missing | | |
| csoundGetControlChannel | available | csnd, str(name)| |
| csoundSetControlChannel | available | csnd, str(name),float(value)||
| csoundGetAudioChannel | missing | | |
| csoundSetAudioChannel | missing | | |
| csoundGetStringChannel |  available | csnd, str(name)| |
| csoundSetStringChannel | available | csnd, str(name), str(value)| |
| csoundGetChannelDatasize | missing | | |
| csoundSetInputChannelCallback | missing | | |
| csoundSetOutputChannelCallback | missing | | |
| csoundSetPvsChannel | missing | | |
| csoundGetPvsChannel | missing | | |
| csoundScoreEvent | missing | | (todo) |
| csoundScoreEventAbsolute | missing | | (todo) |
| csoundInputMessage | available | csnd, str(message) | |
| csoundKillInstance
| csoundRegisterSenseEventCallback
| csoundKeyPress
| csoundRegisterKeyboardCallback
| csoundRemoveKeyboardCallback

## Score Handling
https://csound.github.io/docs/api/group__SCOREHANDLING.html

| API function      | Status        | Parameters |  Comment |
| ----------------- |:-------------:|:---------- |:-------- |
| csoundReadScore | available | csnd, str(score) | |
| csoundGetScoreTime | available | csnd | |
| csoundIsScorePending | availavle | csnd | |
| csoundSetScorePending | available | csnd, int(pending) | |
| csoundGetScoreOffsetSeconds | available | csnd | |
| csoundSetScoreOffsetSeconds | available | csnd, float(time) | |
| csoundRewindScore | available | csnd | |
| csoundSetCscoreCallback | missing | | |
| csoundScoreSort | missing | |  |
| csoundScoreExtract | missing | | |

## Constants
Defined in csound.h

| Symbol                 | Int value  |
| ---------------------- |:----------:|
| CSOUND_CONTROL_CHANNEL |1      |
| CSOUND_AUDIO_CHANNEL   | 2      |
| CSOUND_STRING_CHANNEL  |      3|
| CSOUND_PVS_CHANNEL     |   4|
| CSOUND_VAR_CHANNEL     |   5|
| CSOUND_CHANNEL_TYPE_MASK |    15|
| CSOUND_INPUT_CHANNEL  |       16|
| CSOUND_OUTPUT_CHANNEL |       32|
| CSOUND_CONTROL_CHANNEL_NO_HINTS | 0 |
| CSOUND_CONTROL_CHANNEL_INT | 1     |
| CSOUND_CONTROL_CHANNEL_LIN | 2     |
| CSOUND_CONTROL_CHANNEL_EXP | 3     |
| CSOUNDINIT_NO_SIGNAL_HANDLER | 1 |
| CSOUNDINIT_NO_ATEXIT | 2 |
