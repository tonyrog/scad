                       // scope 1
 a = 6;                // create a
 echo(a,b);            //                6, undef
 translate([5,0,0]){   // scope 1.1
   a= 10;
   b= 16;              // create b
   echo(a,b);          //              100, 16   a=10; was overridden by later a=100;
   color("blue") {     // scope 1.1.1
     echo(a,b);        //              100, 20
     cube();
     b=20;
   }                   // back to 1.1
   echo(a,b);          //              100, 16
   a=100;              // override a in 1.1
 }                     // back to 1   
 echo(a,b);            //                6, undef
 color("red"){         // scope 1.2
   cube();
   echo(a,b);          //                6, undef
 }                     // back to 1
 echo(a,b);            //                6, undef
  
 //In this example, scopes 1 and 1.1 are outer scopes to 1.1.1 but 1.2 is not.

