function makeAudioGraph() {
    ac = new AudioContext();
    channelMerger = ac.createChannelMerger();
    finalGainNode = ac.createGain();
    channelMerger.connect(finalGainNode);
    finalGainNode.connect(ac.destination);
    return { 'ac': ac, 'channelMerger': channelMerger, 'finalGainNode': finalGainNode, 'keymap': {} };
}

function setNoteConfig(graph, notesCfg) {
    toDelete = [];
    for (symbol in graph.keymap) {
        key = graph.keymap[symbol];
        if (symbol in notesCfg) {
            key.oscillator.frequency.value = notesCfg[symbol].freq;
            key.gainNode.gain.value = 0;
            key.maxGain = notesCfg[symbol].gain; // possible for the note to change pitch as it is playing, and be using the wrong gain
        } else {
            key.gainNode.disconnect();
            key.oscillator.disconnect();
            toDelete.add(symbol);
        };
    }
    for (symbol in toDelete) {
        delete(graph.keymap[symbol]);
    }

    for (symbol in notesCfg) {
        if (!(symbol in graph.keymap)) {
            oscillator = graph.ac.createOscillator();
            oscillator.frequency.value = notesCfg[symbol].freq;
            gainNode = graph.ac.createGain();
            gainNode.gain.value = 0;
            oscillator.connect(gainNode);
            gainNode.connect(graph.channelMerger);
            oscillator.start()
            graph.keymap[symbol] = { 'gainNode': gainNode, 'oscillator': oscillator, 'maxGain': notesCfg[symbol].gain, 'on': false }
        }
    }
}

function setActiveNotes(graph, notes) {
    totalAmplitude = 0;
    for (symbol in graph.keymap) {
        key = graph.keymap[symbol];
        if (notes.includes(symbol) && key.on) {
            totalAmplitude += key.maxGain;
        } else if (notes.includes(symbol) && !key.on) {
            totalAmplitude += key.maxGain;
            rampGain(key.gainNode, key.maxGain, key.maxGain);
            key.on = true;
        } else if (!(notes.includes(symbol)) && key.on) {
            rampGain(key.gainNode, 0, key.maxGain);
            key.on = false;
        } else {
            // off and stays off
        }
    }
    if (totalAmplitude > 0.5) {
        finalGain = 0.5 / totalAmplitude;
    } else if (totalAmplitude > 0) {
        finalGain = 1;
    } else {
        finalGain = 0;
    }
    rampGain(graph.finalGainNode, finalGain, 1);

}

async function rampGain(gainNode, targetGain, maxGain) {
    gainNode.gain.cancelScheduledValues(gainNode.context.currentTime);
    changeRate = maxGain / 0.015;
    oldGain = gainNode.gain.value;
    targetTime = Math.max(0.015, gainNode.context.currentTime + Math.abs(targetGain - oldGain) / changeRate);
    gainNode.gain.setValueAtTime(oldGain, gainNode.context.currentTime);

    gainNode.gain.linearRampToValueAtTime(targetGain, targetTime);
}