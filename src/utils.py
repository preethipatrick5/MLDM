from pyswip import Prolog, Variable, Functor
import os
import numpy as np
import random
import time
def learn(file_name):
	a=[]
	b=[]
	prolog = Prolog()
	program_name=file_name[:-3]
	#print(program_name)
	prolog.consult(file_name)
	a=list(prolog.query("induce(program_name)"))
	b=list(prolog.query("aleph:write_rules('theory.txt',program_name)"))
	
def generate_theory_aleph(file_name):
    f = open("theory.txt")
    theory=[]
    for line in f.read().splitlines():
        theory.append(line)
    flag_1="true"
    string=""
    hypo=[]
    for i in theory:
        if flag_1=="true":
            if i[-1]==".":
                string=string+i[0:-1]
                hypo.append(string)
                #print(hypo)
                string=""
            else:
                string=string+i
    return hypo

def generate_theory_metagol(file_name):
    f = open("theory.txt")
    theory=[]
    for line in f.read().splitlines():
        theory.append(line)
    return theory

def evaluate_theory(file_name,hypo):
    prolog = Prolog()
    prolog.consult(file_name)
    f = open(file_name)
    pos_start=0
    pos_end=0
    neg_start=0
    neg_end=0
    for i, line in enumerate(f.read().splitlines()):
        if line==":-begin_in_pos.":
            pos_start=i
        if line==":-end_in_pos.":
            pos_end=i
        if line==":-begin_in_neg.":
            neg_start=i
        if line==":-end_in_neg.":
            neg_end=i
    f = open(file_name)
    all_lines_variable = f.readlines()
    pos_examples=[]
    for i in range(pos_start+1,pos_end):
        pos_examples.append(all_lines_variable[i][0:-2])
    neg_examples=[]
    for i in range(neg_start+1,neg_end):
        neg_examples.append(all_lines_variable[i][0:-2])
    pos_length=len(pos_examples)
    neg_length=len(neg_examples)
    for i in hypo:
        pos_count=0
        neg_count=0
        #print(i)
        prolog.assertz(i)
    for i in pos_examples:
        a=[]
        a=list(prolog.query(i))
            #print(a)
        if len(a)>0:
            pos_count=pos_count+1
    for i in neg_examples:
        a=[]
        a=list(prolog.query(i))
        if len(a)<1:
            neg_count=neg_count+1
            #print(a)
            #print(len(a))
    #print(pos_count, neg_count)
    #print(pos_length,neg_length)
    acc=(pos_count+neg_count)/(pos_length+neg_length)
    #print("Accuracy", acc*100)
    prolog = Prolog()
    return acc


def evaluate_theory_1(file_name,test_file, hypo):
    prolog = Prolog()
    prolog.consult(file_name)
    f = open(file_name)
    f1=open(test_file)
    pos_start=0
    pos_end=0
    neg_start=0
    neg_end=0
    for i, line in enumerate(f1.read().splitlines()):
        if line==":-begin_in_pos.":
            pos_start=i
        if line==":-end_in_pos.":
            pos_end=i
        if line==":-begin_in_neg.":
            neg_start=i
        if line==":-end_in_neg.":
            neg_end=i
    f1 = open(test_file)
    all_lines_variable = f1.readlines()
    pos_examples=[]
    for i in range(pos_start+1,pos_end):
        pos_examples.append(all_lines_variable[i][0:-2])
    neg_examples=[]
    for i in range(neg_start+1,neg_end):
        neg_examples.append(all_lines_variable[i][0:-2])
    pos_length=len(pos_examples)
    neg_length=len(neg_examples)
    for i in hypo:
        pos_count=0
        neg_count=0
        #print(i)
        prolog.assertz(i)
    for i in pos_examples:
        a=list(prolog.query(i))
            #print(a)
        if len(a)>0:
            pos_count=pos_count+1
    for i in neg_examples:
        a=list(prolog.query(i))
        if len(a)<1:
            neg_count=neg_count+1
            #print(a)
            #print(len(a))
    #print(pos_count, neg_count)
    #print(pos_length,neg_length)
    acc=(pos_count+neg_count)/(pos_length+neg_length)
    #print("Accuracy", acc*100)
    return acc

def aleph_pos_neg(file_pos_ex, file_neg_ex):
    file_pos = open(file_pos_ex, 'r')
    #Lines = file1.readlines()
    count=0
    pos_ex=[]
    for line in file_pos.read().splitlines():
        count += 1
        #print("Line{}: {}".format(count, line.strip()))
        pos_ex.append(str(line))
    pos_size=len(pos_ex)
    #print(pos_size)
    file_neg = open(file_neg_ex, 'r')
    #Lines = file1.readlines()
    count=0
    neg_ex=[]
    for line in file_neg.read().splitlines():
        count += 1
        #print("Line{}: {}".format(count, line.strip()))
        neg_ex.append(str(line))
    neg_size=len(neg_ex)
    #print(neg_size)
    #folds_pos = np.array_split(pos_ex, CV)
    #folds_neg = np.array_split(neg_ex, CV)
    return pos_ex, neg_ex

"""
def aleph_cross_val_score(model, pos, neg, cv, scoring='accuracy'):
    train_acc=[]
    test_acc=[]
    time_learn=[]
    folds_pos = np.array_split(pos, cv)
    #random.shuffle(folds_pos)
    folds_neg = np.array_split(neg, cv)
    #random.shuffle(folds_neg)
    for i in range(0,cv):
        pos_fold_ex=[]
        neg_fold_ex=[]
        pos_test_fold_ex=[]
        neg_test_fold_ex=[]
        #print("CV", i+1)
        for j in range(0,cv):
            if j!=i:
                fold=j%cv
                #print("\t",fold+1)
                for pos in folds_pos[fold]:
                    pos_fold_ex.append(pos)
                for neg in folds_neg[fold]:
                    neg_fold_ex.append(neg)
            else:
                fold=j%cv
                for pos in folds_pos[fold]:
                    pos_test_fold_ex.append(pos)
                for neg in folds_neg[fold]:
                    neg_test_fold_ex.append(neg)
        file_1 = open('train.pl', 'w')
        file_3 = open('test.pl', 'w')
        file_2 = open(model, 'r')
        for line in file_2.readlines():
            file_1.write(line)
            file_1.write
        file_1.write("\n:-begin_in_pos.")
        for i in pos_fold_ex:
            file_1.write("\n"+str(i))
        file_1.write("\n:-end_in_pos.")
        file_1.write("\n:-begin_in_neg.")
        for i in neg_fold_ex:
            file_1.write("\n"+str(i))
        file_1.write("\n:-end_in_neg.")
        file_1.close()

        file_3.write("\n:-begin_in_pos.")
        for i in pos_test_fold_ex:
            file_3.write("\n"+str(i))
        file_3.write("\n:-end_in_pos.")
        file_3.write("\n:-begin_in_neg.")
        for i in neg_test_fold_ex:
            file_3.write("\n"+str(i))
        file_3.write("\n:-end_in_neg.")
        file_3.close()
        file_name_1="train.pl"
        file_name_2="test.pl"
        start_time = time.time()
        learn(file_name_1)
        time_learn.append(time.time() - start_time)
        theory=generate_theory_aleph(file_name_1)
        print(theory)
        #print(len(theory))
        acc=evaluate_theory(file_name_1, theory)
        #print("accuracy Train : ", acc)
        #train_acc.append(acc)
        acc=evaluate_theory_1(file_name_1,file_name_2, theory)
        #print("accuracy Test: ", acc)
        test_acc.append(acc)
        file_2.close()
        file_1.close()
    time_1=np.mean(np.array(time_learn))
    return np.array(test_acc), time_1
"""
def aleph_cross_val_score_1(model,  cv, scoring='accuracy'):
    train_acc=[]
    test_acc=[]
    time_learn=[]
    #folds_pos = np.array_split(pos, cv)
    #random.shuffle(folds_pos)
    #folds_neg = np.array_split(neg, cv)
    #random.shuffle(folds_neg)
    for i in range(1,cv+1):
        #print("Fold",i)
        pos_fold_ex=[]
        neg_fold_ex=[]
        pos_test_fold_ex=[]
        neg_test_fold_ex=[]
        #print("CV", i+1)
        for j in range(1,cv+1):
            if j!=i:
                
                fold=j%(cv+1)
                #print("\t Train", fold)
                #print("\t",fold+1)
                pos="split"+str(fold)+".f"
                neg="split"+str(fold)+".n"
                pos_ex, neg_ex=aleph_pos_neg(pos, neg)
                #print(neg_ex)
                pos_fold_ex=pos_fold_ex+pos_ex
                neg_fold_ex=neg_fold_ex+neg_ex
                #neg_fold_ex.append(neg_ex)
                #for pos in folds_pos[fold]:
                    #pos_fold_ex.append(pos)
                #for neg in folds_neg[fold]:
                    #neg_fold_ex.append(neg)
            else:
                
                fold=j%(cv+1)
                #print("\t Test", fold)
                pos="split"+str(fold)+".f"
                neg="split"+str(fold)+".n"
                pos_ex, neg_ex=aleph_pos_neg(pos, neg)
                pos_test_fold_ex=pos_test_fold_ex+pos_ex
                neg_test_fold_ex=neg_test_fold_ex+neg_ex
                #
                #pos_test_fold_ex.append(pos_ex)
                #neg_test_fold_ex.append(neg_ex)
                #for pos in folds_pos[fold]:
                    #pos_test_fold_ex.append(pos)
                #for neg in folds_neg[fold]:
                    #neg_test_fold_ex.append(neg)
        #print("pos train \n", pos_fold_ex)
        #print("neg train\n", neg_fold_ex)
        #print("pos test\n", pos_test_fold_ex)
        #print("neg test\n", neg_test_fold_ex)
        file_1 = open('train.pl', 'w')
        file_3 = open('test.pl', 'w')
        file_2 = open(model, 'r')
        for line in file_2.readlines():
            file_1.write(line)
            file_1.write
        file_1.write("\n:-begin_in_pos.")
        for i in pos_fold_ex:
            file_1.write("\n"+str(i))
        file_1.write("\n:-end_in_pos.")
        file_1.write("\n:-begin_in_neg.")
        for i in neg_fold_ex:
            file_1.write("\n"+str(i))
        file_1.write("\n:-end_in_neg.")
        file_1.close()

        file_3.write("\n:-begin_in_pos.")
        for i in pos_test_fold_ex:
            file_3.write("\n"+str(i))
        file_3.write("\n:-end_in_pos.")
        file_3.write("\n:-begin_in_neg.")
        for i in neg_test_fold_ex:
            file_3.write("\n"+str(i))
        file_3.write("\n:-end_in_neg.")
        file_3.close()
        file_name_1="train.pl"
        file_name_2="test.pl"
        start_time = time.time()
        learn(file_name_1)
        time_learn.append(time.time() - start_time)
        theory=generate_theory_aleph(file_name_1)
        print(theory)
        #print(len(theory))
        acc1=evaluate_theory(file_name_1, theory)
        #print("\taccuracy Train : ", acc1)
        train_acc.append(acc1)
        acc=evaluate_theory_1(file_name_1,file_name_2, theory)
        #print("\taccuracy Test: ", acc)
        test_acc.append(acc)
        file_2.close()
        file_1.close()
    time_1=np.mean(np.array(time_learn))
    return np.array(test_acc), time_1