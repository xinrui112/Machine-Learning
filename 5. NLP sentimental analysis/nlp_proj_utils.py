# -*- coding: utf-8 -*-
import os
import tarfile
import pandas as pd
import wget
from pandas import DataFrame

from functools import partial
from typing import NamedTuple, Tuple


DataMeta = NamedTuple("DataMeta", [("name", str), ("url", str)])

_IMDB = DataMeta(
    name="imdb", url="https://www.dropbox.com/s/l9pj9hy2ans3phi/imdb.tar.gz?dl=1"
)


def _download_data_if_needed(data_meta: DataMeta) -> str:
    """
    Download and extract dataset if needed and return the path to the dataset
    """
    path = os.path.join("resources", data_meta.name)
    zip_path = path + ".tar.gz"

    if os.path.exists(path):
        print("data already available, skip downloading.")
    else:
        print("start downloading...")
        wget.download(data_meta.url, zip_path)

        print("start extracting compressed files...")
        with tarfile.open(zip_path) as tar:
            def is_within_directory(directory, target):
                
                abs_directory = os.path.abspath(directory)
                abs_target = os.path.abspath(target)
            
                prefix = os.path.commonprefix([abs_directory, abs_target])
                
                return prefix == abs_directory
            
            def safe_extract(tar, path=".", members=None, *, numeric_owner=False):
            
                for member in tar.getmembers():
                    member_path = os.path.join(path, member.name)
                    if not is_within_directory(path, member_path):
                        raise Exception("Attempted Path Traversal in Tar File")
            
                tar.extractall(path, members, numeric_owner=numeric_owner) 
                
            
            safe_extract(tar, "resources")
        os.remove(zip_path)

        print("data files are now available at %s" % path)
    return path


def _get_train_test_df(data_meta: DataMeta) -> Tuple[DataFrame, DataFrame]:
    path = _download_data_if_needed(data_meta)
    train, test = tuple(
        pd.read_csv(os.path.join(path, file)) for file in ["train.csv", "test.csv"]
    )
    print("{} loaded successfully.".format(data_meta.name))
    return train, test


get_imdb_dataset = partial(_get_train_test_df, _IMDB)
