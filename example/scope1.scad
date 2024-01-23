// openscad --export-format echo -o - scope1.scad

module foo()
{
    a = 1;    
    b = 1;   
    b = a + 1;
    a = 3;
    echo ("foo:",a=a, b=b);        
}

module bar()
{
    b = 1;    
    a = 1;    
    b = a + 1;
    a = 3;
    echo ("bar:",a=a, b=b);        
}

module baz()
{
    module m1() {
	a = 1;    
	b = a + 1;
	a = 3;
	echo ("m1:",a=a, b=b, c=c);        
    }
    a = 1;    
    b = a + 1;
    c = 2;
    a = 3;
    echo ("bar:",a=a, b=b, c=c);
    m1();
}

foo();
bar();
baz();
