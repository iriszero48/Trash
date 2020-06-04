a=imread('Z:\原图\1.jpg');
subplot(2,1,1);imshow(a);title('原图');
subplot(2,1,2);imshow(imcrop(a,[0 0 100 100]));title('剪切');
