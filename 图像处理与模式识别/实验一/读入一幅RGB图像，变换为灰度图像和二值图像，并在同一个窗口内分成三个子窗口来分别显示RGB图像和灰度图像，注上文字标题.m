a=imread('Z:\原图\1.jpg');
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imshow(rgb2gray(a));title('灰度图像');
subplot(2,2,3);imshow(im2bw(a));title('二值图像');
