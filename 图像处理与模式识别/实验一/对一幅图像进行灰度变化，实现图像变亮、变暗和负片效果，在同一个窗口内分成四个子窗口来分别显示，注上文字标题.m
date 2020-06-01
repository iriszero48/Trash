a=imread('Z:\原图\1.jpg');
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imshow(imadjust(a,[0 1],[0 1],0.1));title('变亮');
subplot(2,2,3);imshow(imadjust(a,[0 1],[0 1],10));title('变暗');
subplot(2,2,4);imshow(imadjust(a,[0 1],[1 0]));title('负片');
