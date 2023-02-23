# base code courtesy of https://compilr.com/ashokdhanavel/infix-to-prefix/conversion.py

def stack_empty(stack):
    return 1 if(len(stack) == 0) else 0
        
def stack_top(stack):
    return stack[-1]

def push_stack(stack,ele):
    stack.append(ele)

def pop_stack(stack):
    return stack.pop()

def operand(opr):
    return 1 if(not(operator(opr)) and (opr != "["
) and (opr != "]"
)) else 0

def operator(opr):
    return 1 if opr in ["&","|","%","-","~","$"] else 0

def precedence(opr):
    if((opr == "~") or (opr == "-")):return(7)
    if(opr == "%"):return(6)
    if(opr == "$"):return(5)
    if(opr == "&"):return(4)
    if(opr == "|"):return(3)
    if(opr == "["):return(2)
    if(opr == "]"):return(1)

def infix_to_prefix(infix_expression):
    prefix_list = []
    stack = []
    lst = list(infix_expression)
    infix_expression = infix_expression[::-1]
    infix_list = list(infix_expression)
    #print lst
    for i in infix_list:
        if operand(i):
            prefix_list.append(i)
        if operator(i):
            while((not(stack_empty(stack))) and (precedence(i) <= precedence(stack_top(stack)))):
                prefix_list.append(stack_top(stack))
                pop_stack(stack)
            push_stack(stack,i)
        if(i == "]"):
            push_stack(stack,i)
        if(i == "["):
            while(stack_top(stack) != "]"):
                append_operator = pop_stack(stack)
                prefix_list.append(append_operator)
            pop_stack(stack)
    while(not(stack_empty(stack))):
        if(stack_top(stack) == "]"):
            pop_stack(stack)
        else:
            prefix_list.append(pop_stack(stack))
    #print prefix_list  

    #return prefix_list
        
    prefix_expression = ''

    for val in prefix_list:
        prefix_expression += val

    return prefix_expression[::-1]


def main():
    infix_expression = raw_input('\nEnter A Valid Expression (0-9 A-Z a-z + - / * ^ $) : ')
    result = infix_to_prefix(infix_expression)
    print('Prefix of given expression: ', result)


if __name__ == '__main__':
    main()
