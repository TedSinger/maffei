# Maffei

## A piano in the browser with configurable layout

Try it at https://fernmyth.net/piano (Chrome, Safari, or Firefox)

``` shell
$ elm make src/*.elm --output=main.js
Success! Compiled 1 module.
$ python -m http.server # or your favorite webserver
Serving HTTP on 0.0.0.0 port 8000 (http://0.0.0.0:8000/) ...
```

### Features

- Change the position of keys with the textbox
- Change the notes played by editing the labels on the keys
- Attempted perceived-loudness equalization

### Misfeatures

- Firefox has a ~~bug~~ _different interpretation of the spec_ in its WebAudio API, resulting in an ugly click on each key press/release. See https://bugzilla.mozilla.org/show_bug.cgi?id=1516108

### Future features?

- Bigger text
- Make labels more obviously editable
- Text area replaced with alternate input method
- Squarish keys
- Have a serious pianist or organist design a sane default layout
- Save layout across sessions
- Record + play-along loop
- Record + export
