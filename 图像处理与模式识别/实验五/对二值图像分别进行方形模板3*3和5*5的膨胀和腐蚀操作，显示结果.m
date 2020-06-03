a=rgb2gray(imread('Z:\原图\1.jpg'));
subplot(2,2,1);imshow(imdilate(a,strel('square',3)));title('膨胀3*3');
subplot(2,2,2);imshow(imdilate(a,strel('square',5)));title('膨胀5*5');
subplot(2,2,3);imshow(imerode(a,strel('square',3)));title('腐蚀3*3');
subplot(2,2,4);imshow(imerode(a,strel('square',5)));title('腐蚀5*5');
