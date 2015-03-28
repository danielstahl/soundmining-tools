
/*
* Instruments used in the MusicPlayerExample
*/
(
SynthDef.new("sine", {
	arg dur = 1, freq = 440, pan = 0, attack = 0.01, amp = 1;
	var sig, env, envGen;
	env = Env.perc(attack, dur - attack);
	sig = EnvGen.kr(env, doneAction: 2) * SinOsc.ar(freq) * amp;
	Out.ar(0, Pan2.ar(sig, pan));
}).add;

SynthDef.new("triangle", {
	arg dur = 1, freq = 440, pan = 0, attack = 0.01, amp = 1;
	var sig, env, envGen;
	env = Env.perc(attack, dur - attack);
	sig = EnvGen.kr(env, doneAction: 2) * LFTri.ar(freq) * amp;
	Out.ar(0, Pan2.ar(sig, pan));
}).add;

SynthDef.new("pulse", {
	arg dur = 1, freq = 440, pan = 0, attack = 0.01, amp = 1;
	var sig, env, envGen;
	env = Env.perc(attack, dur - attack);
	sig = EnvGen.kr(env, doneAction: 2) * Pulse.ar(freq) * amp;
	Out.ar(0, Pan2.ar(sig, pan));
}).add;
)



(
Synth("triangle", [\freq, 400, \dur, 1, \pan, -1, \amp, 0.5]);
Synth("sine", [\freq, 440, \dur, 3, \attack, 0.7, \pan, 1, \amp, 0.2]);
Synth("pulse", [\freq, 330, \dur, 3, \attack, 0.7, \pan, 0, \amp, 0.01]);
)





