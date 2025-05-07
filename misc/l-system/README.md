## About
This project is a visualization of [Lindenmayer Systems (l-systems)](http://paulbourke.net/fractals/lsys/). Essentially they are a way to iteratively rewrite some information using a lookup table. For example, say we have the rule that `O -> Ox`. Then the iterations may appear as follows: `O_O -> Ox_Ox -> Oxx_Oxx -> Oxxx_Oxxx -> ...`. Once the string is grown, a program interprets it as drawing commands, producing fractal-like patterns.

The program was built and tested with python 3.5. It uses `pyglet` for OpenGL integration and `numpy` for array operations.

## File Structure
- `lsystems.json` contains information to build lsystems. It is a json file with the following format:
    - [lsystem-name]: object assigning lsystem information to a name
        - axiom: starting string
        - rules: object of rewriting rules; each key is replaced with its associated value
        - theta: at what angle are left/right turns made
- `property.json` stores default settings for visualizing lsystems
- `lsystem.py` where most of the code lies

## Running
```
python3 -m venv env
source env/bin/activate
python3 -m pip install --user -r requirements.txt
python3 main.py
```

## Examples
![Rings](/assets/l-system-1.gif)
![Board](/assets/l-system-2.gif)
![Sierpinski Arrowhow](/assets/l-system-3.gif)
