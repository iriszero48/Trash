c=imread('Z:\原图\兔兔.jpg');
[cc,x]=imhist(c);
d=histeq(a,cc);
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imhist(a);title('原图直方图');
subplot(2,2,3);imshow(d);title('规定化');
subplot(2,2,4);imhist(d);title('规定化直方图');
