# -*- coding: utf-8 -*-
from __future__ import division  #保留小数的除法
import re
import os
import math  #log
import string

N = 300  #文档总数

#words = [];  #单词
TF = {};  #单词词频
Nx = {};  #单词在所有文档中出现的次数
IDF = {};  #单词逆文本频率
T_I = {};  

Dict = {};

#拆分单词 初步建立词典
def read_word(file_name):
    word_num = 0
    words = []
    file = open(file_name)
    for line in file.readlines():  #返回所有行
        line_Array = line.strip().split()
        for word in line_Array:
            word = word.strip(string.punctuation)
            #if(re.match(r'[^0-9]+', word))  #去除数字
            if word != ' ':
                word_num += 1
                words.append(word.lower())
    file.close()
    return words, word_num

#统计词频
def Freq_word(words, word_num, i):
    Dict_word = {}
    for word in words:
        if Dict_word.has_key(word):
            Dict_word[word] = Dict_word[word] + 1
        else:
            Dict_word[word] = 1
    for word in Dict_word:
        TF[word, i] =  Dict_word[word] / word_num  #计算词频
        if Nx.has_key(word):
            Nx[word] += 1
        else:
            Nx[word] = 1
    return 

def IDF_word():
    for word in Nx:
        IDF[word] = math.log(N / Nx[word])
    return 

def TF_IDF():
    for i in range(0, 299):
        for word in Nx:
            if TF.has_key((word, i)):
                T_I[word, i] = TF[word, i] * IDF[word]
            else:
                T_I[word, i] = 0;
            print(str(T_I[word, i]) + " ")
        print("\n")
    return

if __name__=="__main__":
    for i in range(0, 299):
        words, word_num = read_word(str(i))
        Freq_word(words, word_num, i)
    IDF_word()
    TF_IDF()


os.system("pause")
