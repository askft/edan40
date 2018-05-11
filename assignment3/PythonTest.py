
# p
print("\nProgram p")
k = 3
n = 16
m = 1
while n - m > 0:
    if m - m // k * k > 0:
        pass
    else:
        print(m)
    m = m + 1

# p1
print("\nProgram p1")
n = 1024
b = 2
m = 1
s = 0
p = 1
while n > 0:
    q = n // b
    r = n - q * b
    print(r)
    s = p * r + s
    p = p * 10
    n = q
print(s)
print()

# p4
print("\nProgram p4")
a = 4
b = 4
s = 3
while a > 0:
    c = a ** s
    d = 2 ** a
    print((c, d))
    a = a - 1
print(str(a) + "\n")

