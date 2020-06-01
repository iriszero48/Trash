a=imread('Z:\原图\1.jpg');
b=imrotate(a,45,'bilinear');
imfreqdomain=@(x) imshow(fftshift(log(1+abs(fft2(rgb2gray(x))))),[]);
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imfreqdomain(a);title('原图傅里叶谱');
subplot(2,2,3);imshow(b);title('旋转');
subplot(2,2,4);imfreqdomain(b);title('旋转傅里叶谱');
