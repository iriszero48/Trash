a=imread('Z:\原图\18.jpg'); %加载图片
b=rgb2gray(a); %灰度
[wid,hei]=size(b); %图片大小
quartimg=zeros(wid/2+1,hei/2+1); %用于存放结果
i1=1;
j1=1;
for i=1:2:wid
 for j=1:2:hei
  quartimg(i1,j1)=b(i,j); %每隔一个像素取一个像素保存到结果矩阵中
  j1=j1+1;
 end
 i1=i1+1;
 j1=1;
end
figure %创建一个用来显示图形输出的一个窗口
imshow(uint8(quartimg)) %把结果限制在255以内并显示图像
