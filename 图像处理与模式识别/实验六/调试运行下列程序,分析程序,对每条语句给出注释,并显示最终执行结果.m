I=imread('Z:\原图\car.jpg'); %读取图片
[y,x,z]=size(I); %获取图片尺寸
myI=double(I); %转为double
%%%%%%%%%%% RGB to HSI  %%%%%%%%
tic   %开始计时
%%%%%%%% Y 方向 %%%%%%%%%%
Blue_y=zeros(y,1); %存放蓝色像素个数
% 遍历每个像素
for i=1:y
    for j=1:x
            if((myI(i,j,1)<=30)&&((myI(i,j,2)<=62)&&(myI(i,j,2)>=51))&&((myI(i,j,3)<=142)&&(myI(i,j,3)>=119))) % 蓝色RGB的灰度范围
               Blue_y(i,1)= Blue_y(i,1)+1;     % y方向上蓝色像素个数+1
           end  
    end       
end
[temp MaxY]=max(Blue_y);        % Y方向车牌区域确定
%%%%% Y方向车牌的开始 %%%%%%
PY1=MaxY;
while ((Blue_y(PY1,1)>=5)&&(PY1>1))
       PY1=PY1-1;
end    
%%%%% end %%%%%%
%%%%% Y方向车牌的结束 %%%%%%
PY2=MaxY;
while ((Blue_y(PY2,1)>=5)&&(PY2<y))
       PY2=PY2+1;
end
%%%%% end %%%%%%
IY=I(PY1:PY2,:,:); % 根据Y截取车牌
%%%%%%%% X 方向 %%%%%%%%%% 
Blue_x=zeros(1,x);           
for j=1:x
    for i=PY1:PY2
            if((myI(i,j,1)<=30)&&((myI(i,j,2)<=62)&&(myI(i,j,2)>=51))&&((myI(i,j,3)<=142)&&(myI(i,j,3)>=119))) % 蓝色RGB的灰度范围
               Blue_x(1,j)= Blue_x(1,j)+1; % x方向上蓝色像素个数+1   
           end  
    end       
end
%%%%% X方向车牌的开始 %%%%%%
PX1=1;
while ((Blue_x(1,PX1)<3)&&(PX1<x))
       PX1=PX1+1;
end    
%%%%% end %%%%%%
%%%%% X方向车牌的结束 %%%%%%
PX2=x;
while ((Blue_x(1,PX2)<3)&&(PX2>PX1))
       PX2=PX2-1;
end
%%%%% end %%%%%%
PX1=PX1-2; % 对车牌区域的修正
PX2=PX2+2;
Plate=I(PY1:PY2,PX1-2:PX2,:); % 根据X，Y截取车牌
t=toc % 读取计时
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure,imshow(I); % 画出原图
figure,plot(Blue_y);grid % 画出y方向上的蓝色像素点个数统计并带网格线
figure,plot(Blue_x);grid % 画出x方向上的蓝色像素点个数统计并带网格线
figure,imshow(IY); % 画出根据Y截取的车牌
