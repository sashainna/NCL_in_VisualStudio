ca/on
multax/on
cu/0.5,0.125,1
LN1=LINE/-4.714286,1.661681,0.266582,2.069196
pl1=pl/0,0,1,0.5
fd=30
$$ any one of the following command should output
$$ the statement   FEDRAT/30 to the output cl or as file
$$  before the motion record.
$$    go/ln1,fd
$$    go/ln1,30
    go/ln1,pl1,fd
$$    go/ln1,pl1,30
