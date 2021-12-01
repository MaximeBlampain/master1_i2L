# Global import
import numpy as np
import matplotlib.pyplot as plt
import pandas as pnda
from sklearn.model_selection import train_test_split
import seaborn as sn
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import confusion_matrix

# K plus proches voisins
from sklearn.neighbors import KNeighborsClassifier

# Arbre de décision
from sklearn import tree

# Réseau de neuronnes
from sklearn.neural_network import MLPClassifier

def label_encode(data):
    # Transforme un type catégorie en entier
    le = LabelEncoder()
    # On récupère tous les noms de catégories possibles
    unique_values = list(data["C"].unique())
    le_fitted = le.fit(unique_values)
    # On liste l'ensemble des valeurs
    values = list(data["C"].values)
    # On transforme les catégories en entier
    values_transformed = le.transform(values)
    # On fait le remplacement de la colonne dans le dataframe d'origine
    data["C"] = values_transformed
    return data

def generate_datasets_from_file(data):
    train, test = train_test_split(data, test_size=0.5)
    x_train = train
    y_train = train['Z']
    del (train['Z'])
    x_test = test
    y_test = test['Z']
    del (test['Z'])
    return x_train, y_train, x_test, y_test


def analyzeData(data):
    # nb exemple & nb caracteristique
    print("Nb exemples, Nb caractéristiques :")
    print(data.shape)
    # statistiques
    print("Statistiques :")
    print(data.describe())
    # nb exemples de chaque classe
    print("Nb exemple de chaque classe :")
    print(data.value_counts())
    # matrice correlation
    print("Matrice de correlation, voir Plot")
    corrMatrix = data.corr()
    sn.heatmap(corrMatrix)
    plt.show()


# K plus proches voisins
def kpp_score(x_train, y_train, x_test, y_test):
    knn_train = KNeighborsClassifier()
    knn_train.fit(x_train, y_train)

    knn_test = KNeighborsClassifier()
    knn_test.fit(x_test, y_test)

    score_train = knn_train.score(x_train, y_train)
    score_test = knn_train.score(x_test, y_test)

    cmatrix = confusion_matrix(y_test, knn_train.predict(x_test))
    return score_train, score_test, cmatrix


# Arbre de décision
def tree_score(x_train, y_train, x_test, y_test):
    tree_train = tree.DecisionTreeClassifier(criterion='entropy')
    tree_train.fit(x_train, y_train)

    tree_test = tree.DecisionTreeClassifier(criterion='entropy')
    tree_test.fit(x_test, y_test)

    score_train = tree_train.score(x_train, y_train)
    score_test = tree_train.score(x_test, y_test)

    cmatrix = confusion_matrix(y_test, tree_train.predict(x_test))
    return score_train, score_test, cmatrix


def neural_score(x_train, y_train, x_test, y_test):
    neural_train = MLPClassifier(random_state=1, max_iter=800).fit(x_train, y_train)
    neural_train.fit(x_train, y_train)

    neural_test = MLPClassifier(random_state=1, max_iter=800).fit(x_train, y_train)
    neural_test.fit(x_test, y_test)

    score_train = neural_train.score(x_train, y_train)
    score_test = neural_train.score(x_test, y_test)

    cmatrix = confusion_matrix(y_test, neural_train.predict(x_test))
    return score_train, score_test, cmatrix



def del_data(data, arr_characters):
    d = data
    for char in arr_characters:
        del d[char]
    return d

def mean_score(ex):
    train, test = ex
    return (train + test) / 2

def main():
    filename = "exam.csv"
    data = pnda.read_csv(filename)
    data = label_encode(data)

    # Analyse des données
    analyzeData(data)

    char_to_delete = ['A', 'B', 'C', 'J', 'N', 'T', 'U', 'V', 'W', 'X', 'Y']
    data = del_data(data, char_to_delete)
    x_train, y_train, x_test, y_test = generate_datasets_from_file(data)

    best_knn = -1, -1
    best_knn_cm = 0
    best_tree = -1, -1
    best_tree_cm = 0
    best_neural = -1, -1
    best_neural_cm = 0

    nb_iterations = 10

    for i in range(nb_iterations):
        # K plus proches voisins
        score_train, score_test, cmatrix = kpp_score(x_train, y_train, x_test, y_test)
        if mean_score((score_train, score_test)) > mean_score(best_knn):
            best_knn = score_train, score_test
            best_knn_cm = cmatrix
        # Arbre de décision
        score_train, score_test, cmatrix = tree_score(x_train, y_train, x_test, y_test)
        if mean_score((score_train, score_test)) > mean_score(best_tree):
            best_tree = score_train, score_test
            best_tree_cm = cmatrix
        # Réseaux de neuronnes
        score_train, score_test, cmatrix = neural_score(x_train, y_train, x_test, y_test)
        if mean_score((score_train, score_test)) > mean_score(best_neural):
            best_neural = score_train, score_test
            best_neural_cm = cmatrix
    best_knn_train, best_knn_test = best_knn
    best_tree_train, best_tree_test = best_tree
    best_neural_train, best_neural_test = best_neural

    print(f"After {nb_iterations} iterations, here's the results ...")
    print("Learning with KNN ...")
    print(f"Score Train: {best_knn_train} / Score Test: {best_knn_test}")
    print("Matrice de confusion :")
    print(best_knn_cm)


    print("Learning with tree ...")
    print(f"Score Train: {best_tree_train} / Score Test: {best_tree_test}")
    print("Matrice de confusion :")
    print(best_tree_cm)


    print("Learning with neural network ...")
    print(f"Score Train: {best_neural_train} / Score Test: {best_neural_test}")
    print("Matrice de confusion :")
    print(best_neural_cm)


if __name__ == '__main__':
    main()

