a=imnoise(rgb2gray(imread('Z:\原图\1.jpg')),'salt & pepper',0.04);
subplot(2,1,1);imshow(a);title('椒盐噪声');
subplot(2,1,2);imshow(filter2(fspecial('average',3),a)/255);title('均值滤波');
