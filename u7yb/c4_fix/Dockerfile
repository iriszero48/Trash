FROM python:3.9
COPY ./src /code
COPY data.txt /code
WORKDIR /code
RUN pip install -r requirements.txt
RUN chmod +x run.sh
#CMD nc -l 8080 --exec ./run.sh
CMD ["/bin/bash"]
