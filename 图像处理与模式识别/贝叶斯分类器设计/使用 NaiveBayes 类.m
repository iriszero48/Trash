%训练数据及其类别
meas=[0 0;2 0;2 2;0 2;4 4;6 4;6 6;4 6];
[N n]=size(meas); species={'one';'one';'one';'one';'two';'two';'two';'two'}; 
ObjBayes=fitcnb(meas,species);
x=[3 3];
result=predict(ObjBayes,x);
result%；
