%训练数据及其类别
meas=[0 0;2 0;2 2;0 2;4 4;6 4;6 6;4 6];
[N n]=size(meas);
species={'one';'one';'one';'one';'two';'two';'two';'two'};
%估算先验概率
sta=tabulate(species);
[c,k]=size(sta);
priorp=zeros(c,1);
for i=1:c
 priorp(i)=cell2mat(sta(i,k))/100;
end
%估算类条件概率参数
cpmean=zeros(c,n);
cpcov=zeros(n,n,c);
for i=1:c
 cpmean(i,:)=mean(meas(strmatch(char(sta(i,1)),species,'exact'),:));
 cpcov(:,:,i)=cov(meas(strmatch(char(sta(i,1)),species,'exact'),:))*(N*priorp(i)-1)/(N*priorp(i));
end
%数据（3 1）的后验概率
x=[3 1];
postp=zeros(c,1);
for i=1:c
 postp(i)=priorp(i)*exp(-(x-cpmean(i,:))* inv(cpcov(:,:,i)) *(x-cpmean(i,:))'/2)/((2*pi)^(n/2)*det(cpcov(:,:,i))); 
end
max=0;
for i=1:c
 if max<postp(i) 
  max=postp(i); 
  result_mis=sta(i,1);
 end
end
result_mis%；
%条件风险
loss11=0;loss12=7;loss21=2;loss22=0;
R(1)=loss11* postp(1)+loss12* postp(2)%属于第一类的条件风险
R(2)=loss21* postp(1)+loss22* postp(2)%属于第二类的条件风险
max=0; 
for i=1:c 
 if max<R(i) 
  max=postp(i); 
  result_ris=sta(i,1);
 end
end
result_ris%；
