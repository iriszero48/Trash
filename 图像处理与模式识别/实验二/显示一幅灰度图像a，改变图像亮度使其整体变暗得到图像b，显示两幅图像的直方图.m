a=rgb2gray(imread('Z:\原图\1.jpg'));
b=imadjust(a,[,],[0;0.5]);
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imhist(a);title('原图直方图');
subplot(2,2,3);imshow(b);title('变暗');
subplot(2,2,4);imhist(b);title('变暗直方图');
