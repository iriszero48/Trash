import os

import cv2
import numpy as np
from pathlib import Path
from keras.applications.imagenet_utils import preprocess_input
from keras.models import load_model


def process_images(input_path,
                   batch_size=64, crop=True,
                   model_path=str(Path(__file__).parent / 'rotnet_models' / 'rotnet_street_view_resnet50_keras2.hdf5')):
    models = OAD()
    models.load_model("")
    extensions = []

    if os.path.isfile(input_path) or input_path[:4].lower() == "http":
        image_paths = [input_path]

    else:
        image_paths = [os.path.join(input_path, f)
                       for f in os.listdir(input_path)
                       if os.path.splitext(f)[1].lower() in extensions]

    img = ImageScale(input_path)
    cv2.imwrite('', img)

    with open('', 'w') as f:
        f.write('')

    predictions = models.predict("", '')[0]
    print(predictions)
    return predictions
