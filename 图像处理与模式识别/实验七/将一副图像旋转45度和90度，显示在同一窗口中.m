a=imread('Z:\原图\1.jpg');
subplot(3,1,1);imshow(a);title('原图');
subplot(3,1,2);imshow(imrotate(a,45));title('45度');
subplot(3,1,3);imshow(imrotate(a,90));title('90度');
