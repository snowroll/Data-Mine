# -*- coding: utf-8 -*-
from __future__ import division  #保留小数的除法
import re
import os
import math  #log
import string

import numpy as np  #矩阵

N = 300  #文档总数

#words = [];  #单词
TF = {};  #单词词频
IDF = {};  #单词逆文本频率
T_I = {};
All_Words = {};  #单词在所有文档中出现的次数  
keyword = [];

#拆分单词 初步建立词典
def read_word(file_name):
    word_num = 0
    words = []
    file = open(file_name)
    for line in file.readlines():  #返回所有行
        line_Array = line.strip().split()
        for word in line_Array:
            word = word.strip(string.punctuation)
            if word != '':
                word_num += 1
                words.append(word.lower())
    file.close()
    return words, word_num

#统计词频
def Freq_word(words, word_num, i):
    Dict_word = {}
    for word in words:
        if Dict_word.has_key(word):
            Dict_word[word] += 1
        else:
            Dict_word[word] = 1
    for word in Dict_word:
        TF[word, i] =  Dict_word[word] / word_num  #计算单词在某篇文章中的词频
        if All_Words.has_key(word):  #记录all_words 是为什么？？
            All_Words[word] += 1
        else:
            All_Words[word] = 1
    return 

def IDF_word():
    for word in All_Words:
        #print(All_Words)
        IDF[word] = math.log(N / All_Words[word])
    return 

def TF_IDF():
    for i in range(0, N):
        for word in All_Words:
            if TF.has_key((word, i)):
                T_I[word, i] = TF[word, i] * IDF[word]
            else:
                T_I[word, i] = 0;
    return T_I

def trans(m):
    for i in range(len(m)):
        for j in range(i):
            m[i][j], m[j][i] = m[j][i], m[i][j]
    return m

#2题  建立共现矩阵  http://blog.csdn.net/alanconstantinelau/article/details/69258443
def build_matrix(word_list):
    #建立矩阵，矩阵的高度和宽度为关键词长度+1
    edge = len(word_list) + 1
    matrix = [['' for j in range(edge)] for i in range(edge)]
    return matrix 

def init_matrix(word_list, matrix):
    #初始化矩阵，将关键词赋值给第一列和第一行
    for word in word_list:
        keyword.append(word)
    matrix[0][1:] = np.array(keyword)
    #matrix = list(map(list, zip(*matrix))) #转置
    matrix = trans(matrix)
    matrix[0][1:] = np.array(keyword)
    matrix[0][0] = str(0)
    for i in range(1, len(matrix)):
        for j in range(1, len(matrix)):
            matrix[i][j] = str(0)
    return matrix  #初始矩阵构建完毕

def count_matrix(matrix, T_I):  #复杂度太高，最外循环 文本 ， 加 判断条件 减少复杂度
    for i in range(0, N):
        for row in range(1, len(matrix)):
            if T_I[matrix[0][row], i] == 0:
                continue
            for col in range(1, len(matrix)):
                counter = 0
                if T_I[matrix[col][0], i] == 0:
                    continue
                counter += 1
                matrix[row][col] = str(counter)
    return matrix




    # for row in range(1, len(matrix)):
    #     for col in range(1, len(matrix)):
    #         if matrix[0][row] == matrix[col][0]:
    #             matrix[col][row] = str(0)  
    #         else:
    #             counter = 0
    #             for i in range(0, N):
    #                 if T_I[matrix[0][row], i] != 0 and T_I[matrix[col][0], i] != 0:
    #                     counter += 1
    #                 else:
    #                     continue
    #             matrix[col][row] = str(counter)
    # return matrix

def sort_print(Order, flag, tag):  #欧式距离 flag = 0 余弦 flag = 1
    _Sort = zip(Order.values(), Order.keys())
    if flag == 1:
        _Sort = sorted(_Sort, reverse = True)  #从大到小
    else:
        _Sort = sorted(_Sort)
    f = open('E:/lesson_file/Junior/Summer-Term/Data-Mining/Homework/nyt_corp0/ans.txt', 'a+')
    f.write(tag)
    f.write("\n")
    i = 0
    for i in range(0, 10):
        if i < 10:
            #print(_Sort[i])
            f.write(_Sort[i].__str__())
            f.write("\n")
            i += 1
        else:
            break
    return 

#第三题　求解距离　余弦求解相关性
def Euclidean_dis():
    Euc_dis = []
    Order_Edist_Doc = {}
    
    for i in range(1, N):
        Euc_dis.append(0)
        for word in All_Words:
            Euc_dis[i-1] += pow((T_I[word,0] - T_I[word,i]), 2)
        Order_Edist_Doc[i] = Euc_dis[i-1]
    tag = "similar doc by Euc"
    sort_print(Order_Edist_Doc, 0, tag)
    return Euc_dis;

def cos_dis():
    Cos_dis = []
    Order_Cos = {}
    for i in range(1,N):
        Cos_dis.append(0)
        Mult, d1, d2 = 0, 0, 0
        
        for word in All_Words: 
            Mult += T_I[word,0] * T_I[word,i]
            d1 += T_I[word,0] * T_I[word,0]
            d2 += T_I[word,i] * T_I[word,i]
        if d1 * d2 == 0:
            Cos_dis[i-1] = 0
        else:
            Cos_dis[i-1] = Mult / math.sqrt(d1 * d2)
        Order_Cos[i] = Cos_dis[i-1]
    tag = "similar doc by Cos"
    sort_print(Order_Cos, 1, tag)
    return Cos_dis

#第四题            
def Euclidean_dis_word(matrix):
    Euc_word_dis = []
    Order_Edist_Word = {}
    for i in range(2, len(matrix)):
        Euc_word_dis.append(0)
        for j in range(1, len(matrix)):
            Euc_word_dis[i-2] += pow((int(matrix[1][j]) - int(matrix[i][j])), 2)
        Order_Edist_Word[keyword[i-1]] = Euc_word_dis[i-2]  #把单词 距离 存下来，方便排序
    tag = "similar word by Euc " + matrix[1][0]
    #print(matrix[1][0] + "000")
    sort_print(Order_Edist_Word, 0, tag)    
    return Euc_word_dis

def Cos_dis_word(matrix):
    cos_word_dist = []
    Order_Cdist_Word = {}
    Mult, d1, d2 = 0, 0, 0
    for i in range(2, len(matrix)):
        cos_word_dist.append(0)
        for j in range(1, len(matrix)):
            Mult += int(matrix[1][j]) * int(matrix[i][j])
            d1 += pow(int(matrix[1][j]), 2)
            d2 += pow(int(matrix[i][j]), 2)
        if d1 * d2 == 0:
            cos_word_dist[i-2] = 0
        else:
            cos_word_dist[i-2] = Mult / math.sqrt(d1 * d2)
        Order_Cdist_Word[keyword[i-1]] = cos_word_dist[i-2]
    tag = "similar word by cos " + matrix[1][0]
    sort_print(Order_Cdist_Word, 1, tag)
    return cos_word_dist



            
    

if __name__=="__main__":
    for i in range(0, N):
        words, word_num = read_word(str(i))
        Freq_word(words, word_num, i)
    IDF_word()
    T_I = TF_IDF()

    #2题
    Ori_Matrix =build_matrix(All_Words)
    Matrix = init_matrix(All_Words,Ori_Matrix)
    Matrix = count_matrix(Matrix, T_I)
    
    #3题
    Euclidean_dis()
    cos_dis()
    
    #4题
    Euclidean_dis_word(Matrix)
    Cos_dis_word(Matrix)    


os.system("pause")


