import cProfile
def main():
    toFind = 73987
    a = 0
    b = 0
    found = 0
    while not found and a < toFind/2:
        a = a+1
        b = 0
        while not found and b < toFind/2:
            b = b + 1
            if a * b == toFind:
                found = 1

cProfile.run("main()")
