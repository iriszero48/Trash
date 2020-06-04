a=rgb2gray(imread('Z:\原图\1.jpg'));
subplot(2,1,1);imshow(a);title('原图');
subplot(2,1,2);imshow(cat(3,(a>=60&a<100)*255,(a>=100&a<150)*255,(a>=150)*255));title('伪彩色');
