__author__ = 'tracy'

import math
import sys, getopt

def readDict(file):
    """
    This function read in file and create two dictionary
    :param file: txt rating file
    :return: two dictionary with value as dictionary either
    """
    movieDict = {}
    userDict = {}
    with open(file) as f:
        for line in f:
            row = (x.strip() for x in line.split(','))
            movie, user, rating = row
            if movie not in movieDict.keys():
                movieDict[movie] = {}
            movieDict[movie][user] = float(rating)
            if user not in userDict.keys():
                userDict[user] = {}
            userDict[user][movie] = float(rating)

    f.close()

    return movieDict, userDict


def pearson(i, j, movieDict, userDict):
    """
    This function calculate the pearson coefficient
    :param i: user i
    :param j: user j
    :return: pearson coefficient
    """
    R_i = sum(userDict[i].values())/len(userDict[i].values())
    R_j = sum(userDict[j].values())/len(userDict[j].values())
    numerator = 0
    denominator1 = 0
    denominator2 = 0

    for k in movieDict.keys():
        R_ik = movieDict[k].get(i)
        R_jk = movieDict[k].get(j)
        if R_ik is not None and R_jk is not None:
            numerator += (R_ik - R_i)*(R_jk - R_j)
            denominator1 += (R_ik - R_i)*(R_ik - R_i)
            denominator2 += (R_jk - R_j)*(R_jk - R_j)
    # consider the situation where all the ratings for user is the same, we manually set w_ij to be 0
    if denominator1 == 0 or denominator2 == 0:
        w_ij = 0
    else:
        w_ij = numerator/math.sqrt(denominator1*denominator2)

    return w_ij

def predict(i, k, movieDict, userDict):
    """
    This function predict one blank rating at a time
    :param i: user i
    :param k: movie k
    :param movieDict: data stored in dict with movies as keys
    :param userDict: data stored in dict with user as keys
    :return: predicted value in blank
    """
    numerator = 0
    denominator = 0
    if i not in userDict.keys() and k not in movieDict.keys():
        R_ik = float(sum([sum(movieDict[k].values()) for k in movieDict.keys()]))/\
           sum([len(movieDict[k].values()) for k in movieDict.keys()])
    elif i not in userDict.keys():
        R_ik = float(sum(movieDict[k].values()))/len(movieDict[k].values())
    elif k not in movieDict.keys():
        R_ik = float(sum(userDict[i].values()))/len(userDict[i].values())


    else:
        for u in movieDict[k].keys():
            R_jk = userDict[u][k]
            R_j = sum(userDict[u].values())/len(userDict[u].values())
            w_ij = pearson(i, u, movieDict, userDict)
            numerator += w_ij*(R_jk - R_j)
            denominator += w_ij
        R_i = sum(userDict[i].values())/len(userDict[i].values())
        if denominator == 0:
            R_ik = R_i
        else:
            R_ik = R_i + float(numerator)/denominator

    return R_ik

def evaluation(testfile, movieDict, userDict):
    """
    This function predict the value that is in the testfile and calculate accuracy
    :param testfile:
    :param movieDict:
    :param userDict:
    :return: mean_error, rmsqr_error
    """
    movieDict_t, userDict_t = readDict(testfile)
    movies = movieDict_t.keys()
    error = 0
    sqr_error = 0
    N = 0
    for k in movies:
        for u in movieDict_t[k].keys():
            R_uk = predict(u, k, movieDict, userDict)
            # write predicted result
            with open('predictions.txt', 'a') as f:
                f.write(k+','+u+','+str(movieDict_t[k][u])+','+str(R_uk)+'\n')

            error += abs(movieDict_t[k][u] - R_uk)
            sqr_error += (movieDict_t[k][u] - R_uk)*(movieDict_t[k][u] - R_uk)
            N += 1.0
    f.close()
    mean_error = error/N
    rmsqr_error = math.sqrt(sqr_error/N)

    return mean_error, rmsqr_error

def main():
    """
    Function for terminal input files
    :return:train_filename, test_filename
    """
    options, remainder = getopt.getopt(sys.argv[1:], '',['train=', 'test='])
    for opt, arg in options:
        if opt in ('--train'):
            train_filename = arg
        if opt in ('--test'):
            test_filename = arg

    return train_filename, test_filename

def newUser(k, movieDict):
    """
    This function predict the rating for a new user a know movie by calculating avg
    :param k: movie
    :return: average movie k ratings
    """
    R = float(sum(movieDict[k].values))/len(movieDict[k].values)
    return R

def newMovie(u, userDict):
    """
    This function predict the rating for a know user a new movie by calculating avg
    :param u: user
    :return: average user u ratings
    """
    R = float(sum(userDict[u].values))/len(userDict[u].values)
    return R



if __name__ == '__main__':
    file1, file2 = main()

    movieDict, userDict = readDict(file1)

    mean_error, rmsqr_error = evaluation(testfile=file2, movieDict=movieDict, userDict=userDict)
    print "mean absolute error = " + str(mean_error)
    print "root mean squared error =" + str(rmsqr_error)


