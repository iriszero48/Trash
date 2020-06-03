a=im2double(imread('Z:\原图\1.jpg'));
b=imfilter(a,fspecial('motion',21,11),'conv','circular');
subplot(1,3,1);imshow(a);title('原图');
subplot(1,3,2);imshow(b);title('运动模糊');
subplot(1,3,3);imshow(deconvwnr(b,fspecial('motion',21,11)));title('维纳滤波');
