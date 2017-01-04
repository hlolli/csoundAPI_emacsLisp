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
| csoundGetOutputName|available*    | csnd       | Crashes emacs instead of returning nil|
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
| csoundGetChannelPtr
| csoundListChannels
| csoundDeleteChannelList
| csoundSetControlChannelHints
| csoundGetControlChannelHints
| csoundGetChannelLock
| csoundGetControlChannel
| csoundSetControlChannel
| csoundGetAudioChannel
| csoundSetAudioChannel
| csoundGetStringChannel
| csoundSetStringChannel
| csoundGetChannelDatasize
| csoundSetInputChannelCallback
| csoundSetOutputChannelCallback
| csoundSetPvsChannel
| csoundGetPvsChannel
| csoundScoreEvent
| csoundScoreEventAbsolute
| csoundInputMessage
| csoundKillInstance
| csoundRegisterSenseEventCallback
| csoundKeyPress
| csoundRegisterKeyboardCallback
| csoundRemoveKeyboardCallback
