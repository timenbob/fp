a = 15
b = 13
def f1(x, y=a):
    def f2(a, c=b):
        return a + b + c
    return f2


print(f1(1)(3))

a = 25
b = 23
print(f1(1)(3))

