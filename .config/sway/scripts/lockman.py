import subprocess as sub

swaylock = sub.Popen("swaylock")

t = 0

while swaylock.poll() is None:
    print(t)
    t += 1
