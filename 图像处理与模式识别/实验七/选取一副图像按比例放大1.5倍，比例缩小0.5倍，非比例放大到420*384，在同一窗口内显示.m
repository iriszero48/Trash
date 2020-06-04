a=imread('Z:\原图\1.jpg');
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imshow(imresize(a,1.5));title('1.5倍');
subplot(2,2,3);imshow(imresize(a,0.5));title('0.5倍');
subplot(2,2,4);imshow(imresize(a,[420 384]));title('420*384');
