a=imread('Z:\原图\1.jpg');
aveFilter=@(x) filter2(fspecial('average',5),x)/255;
r=aveFilter(a(:,:,1));
g=aveFilter(a(:,:,2));
b=aveFilter(a(:,:,3));
subplot(3,2,1);imshow(a);title('原图'); 
subplot(3,2,3);imshow(r);title('R'); 
subplot(3,2,4);imshow(g);title('G');
subplot(3,2,5);imshow(b);title('B');
subplot(3,2,6);imshow(cat(3,r,g,b));title('三分量合成');
