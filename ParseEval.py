import re
ops=["^","*","/","-","+"]
spec=["(",")"]
def simplify(s):
    s = re.sub("\\s+","",s)
    s = re.sub("(-\\+|\\+-)+", "-",s)
    s = re.sub("(-{2})+|\\++", "",s)
    s = re.sub("(-\\s*\\+|\\+\\s*-|-)+", "-",s)
    s = re.sub("\\*(\\s*|\\++)", "*",s)
    s = re.sub("/(\\s*|\\++)", "/",s)
    s = re.sub("\\s*\\^\\s*", "^",s)
    return s
def calc(p):
    p=simplify(p)
    p=re.sub("(?<=[\(\b ])\-","0-",p)
    s=p
    shell=peel(p)
    if len(shell)!=0:
        for l in shell:
            pattern="("+s[l[0]:l[1]]+")"
            for i in ops+spec:
                pattern=re.sub("\\"+i,"\\\\"+i,pattern)
            p=re.sub(pattern,calc(s[l[0]:l[1]]),p)
        return calc(p)
    else:
        return evaluate(p)
def peel(s):
    l=0
    layer=[]
    v=[]
    for i in range(len(s)):
        if(s[i]=="("):
            if(l==0):
                v.append(i+1)
            l+=1
        if(s[i]==")"):
            l-=1
            if(l==0):
                v.append(i)
        if len(v)==2:
            layer.append(v)
            v=[]
    if len(layer)!=0:
        return layer
    return []
def evaluate(s):
    v=0
    if s[0]!="(":
        s="("+s
    if s[len(s)-1]!=")":
        s=s+")"
    while(re.search("[\^\/\*\-\+]",s)):
        l=sorted(re.findall("(?=(?:((?<=[\(\*\-\+\/\^])\-?[\d\.]+)([\*\-\/\+\^])(\-?[\d\.]+)))",s),key=lambda layer:ops.index(layer[1]))  
        if len(l)==0:
            break
        i=l[0]
        if i[1]=="*":
            v=float(i[0])*float(i[2])
        elif  i[1]=="/":
            v=float(i[0])/float(i[2])
        elif  i[1]=="+":
            v=float(i[0])+float(i[2])
        elif  i[1]=="-":
            v=float(i[0])-float(i[2])
        elif  i[1]=="^":
            v=float(i[0])**float(i[2])
        pattern=re.sub('\\'+i[1],"\\\\"+i[1],"".join([j for j in i]))
        s=re.sub(pattern,str(v),s)
    v=s[1:len(s)-1]
    return v
def f():
    s=input(">")
    l_input=list(set(re.findall("([a-z])",s)))
    if len(l_input)!=0:
        x={}
        for i in range(len(l_input)):
            i,x[i]=input().split()
        for y in x:
            s=re.sub(y,x[y],s)
        return calc(s)
    else:
        return calc(s)
if __name__ == "__main__":
    while True:
        print(f())
