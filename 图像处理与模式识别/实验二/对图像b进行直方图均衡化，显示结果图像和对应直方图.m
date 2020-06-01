c=histeq(b);
subplot(2,2,1);imshow(b);title('原图');
subplot(2,2,2);imhist(b);title('原图直方图');
subplot(2,2,3);imshow(c);title('均衡化');
subplot(2,2,4);imhist(c);title('均衡化直方图');
