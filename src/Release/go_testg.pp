ca/on
multax/on
cu/0.5,0.125,1

$$ fd=25
LN1=LINE/-4.46875,-1.197956,-4.300383,1.84436
LN2=LINE/-4.426658,1.689785,0.940051,1.886517
LN3=LINE/0.652423,2.090275,1.71875,-1.471975
LN4=LINE/1.830995,-0.937989,-4.552934,-1.760046
PL1=pl/0,0,1,1
cu/0.5,0,1
$$go/ln1,50
go/ln3,pl1,30
$$go/ln1,pl1,ln4,25   
$$ output vocab word reguired after ln4, this works before 