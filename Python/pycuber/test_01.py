import pycuber as pc

cube = pc.Cube()
print(cube)

cube("U U L R' F F D R L U D")
print(repr(cube))

print(cube.is_valid())

