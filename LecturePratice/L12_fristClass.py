# add 함수 생성 
def add(num1, num2):
    return num1 + num2

# add함수를 func변수에 할당
var = add

# add 함수를 var 변수에 할당결과 같은 메모리 주소값을 바라봄을 알 수 있다.
print(add) ## <function add at 0x00000218AE94DD30>
print(var) ## <function add at 0x00000218AE94DD30>
print(add is var) ### True

# 변수에 함수 할당이 가능함을 알 수 있다.
print(add(2, 3)) ## 5
print(var(2,3)) ## 5
print(add(2, 3) == var(2,3)) ## True
print(add(2, 3) is var(2,3)) ## True

def square(x):
 return x * x

def first_class(func, arg_list):
 print(f"Running Function : {func.__name__}") ## Running Function : square

 # func(i) is square(i)
 return [func(i) for i in arg_list]

num_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
result = first_class(square, num_list)
print(result) ## [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

"""
리스트 컴프리헨션(지능형 리스트)을 모르는 분들을 위해 위 first_class 함수는 다음과 같다

def first_class(func, arg_list):
 print(f"Running Function : {func.__name__}")
 square_list = []
 for i in range arg_list:
     square_list.append(func(i))

 return square_list
"""

"""
Scopeing example
"""
# static x (outer scope)
first_x_of_theScope = 3

def f(arg):
    return arg + first_x_of_theScope # in our case, we want x to use by 3 

# x for dynamic scope (inner scope)
x = 5
print("f({}) : {}".format(4,f(4))) # return value is 7
x = 10 
print("f({}) : {}".format(4,f(4))) # return value is 7
x = 97
print("f({}) : {}".format(4,f(4))) # return value is 7



