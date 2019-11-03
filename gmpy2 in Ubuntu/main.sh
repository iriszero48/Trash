mkdir -p $HOME/src
mkdir -p $HOME/static

apt install m4
apt-get install python-dev
echo 52.216.16.16 github-production-release-asset-2e65be.s3.amazonaws.com >>/etc/hosts
/etc/init.d/networking restart

v=6.1.2
cd $HOME/src
wget https://gmplib.org/download/gmp/gmp-${v}.tar.bz2
tar -jxvf gmp-${v}.tar.bz2 && cd gmp-${v}
./configure --prefix=$HOME/static --enable-static --disable-shared --with-pic
make && make check && make install

v=4.0.1
cd $HOME/src
#wget http://www.mpfr.org/mpfr-current/mpfr-${v}.tar.bz2
wget http://ftp.gnu.org/gnu/mpfr/mpfr-${v}.tar.bz2
tar -jxvf mpfr-${v}.tar.bz2 && cd mpfr-${v}
./configure --prefix=$HOME/static --enable-static --disable-shared --with-pic --with-gmp=$HOME/static
make && make check && make install

v=1.1.0
cd $HOME/src
wget ftp://ftp.gnu.org/gnu/mpc/mpc-${v}.tar.gz
tar -zxvf mpc-${v}.tar.gz && cd mpc-${v}
./configure --prefix=$HOME/static --enable-static --disable-shared --with-pic --with-gmp=$HOME/static --with-mpfr=$HOME/static
make && make check && make install

v=2.1.0a1
cd $HOME/src
wget https://github.com/aleaxit/gmpy/releases/download/gmpy2-2.1.0a1/gmpy2-${v}.tar.gz
tar xf gmpy2-${v}.tar.gz && cd gmpy2-${v}
python setup.py build_ext --static=$HOME/static install
