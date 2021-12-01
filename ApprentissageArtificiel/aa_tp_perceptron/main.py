import numpy as np
from typing import NamedTuple

def generate_line(filename, nb_lines) :
    file = open(filename, "w")
    for index in range(nb_lines):
        x1 = np.random.random()
        x2 = np.random.random()
        line_to_write = add_tag(x1, x2)
        line_to_write += "\n"
        file.write(line_to_write)
    file.close()


def add_tag(x1, x2):
    if x1 + x2 - 1 > 0:
        tag_value = 1
    else:
        tag_value = -1

    line_list = [str(x1), str(x2), str(tag_value)]
    separator = " "
    line_result = separator.join(line_list)
    return line_result


def read_lines(filename):
    file = open(filename, "r")
    lines = file.readlines()
    file.close()
    return lines


class Point(NamedTuple):
    x1: float
    x2: float
    tag: int


class Neurone(NamedTuple):
    biais: float
    output: int
    weights: list


def init_neurone():
    return Neurone(0.5, 0, [np.random.random(), np.random.random()])


def calc_output_neurone(point, neurone):
    calc = (neurone.weights[0] * point.x1 + neurone.weights[1] * point.x2)-neurone.biais
    if calc > 0:
        result = 1
    else:
        result = -1

    return result


def update_neurone(neurone, point, pas_apprentissage):
    new_biais = neurone.biais + pas_apprentissage * (point.tag - neurone.output) * (-0.5)
    new_w1 = neurone.weights[0] + pas_apprentissage * (point.tag - neurone.output) * point.x1
    new_w2 = neurone.weights[1] + pas_apprentissage * (point.tag - neurone.output) * point.x2
    return Neurone(new_biais, neurone.output, [new_w1, new_w2])


def main():
    np.random.seed(1337)

    # Set nb_points_train & nb_points_test
    nb_points_train = 100
    nb_points_test = 30

    # Generate random array
    generate_line("train.txt", nb_points_train)
    generate_line("test.txt", nb_points_test)

    ################# Neurone #################
    pas_apprentissage = 0.01

    # Boucle principale
    nb_errors = 0
    my_neurone = init_neurone()
    lines = read_lines("train.txt")
    for line in lines:
        actual_line = line.split()
        dot = Point(float(actual_line[0]), float(actual_line[1]), int(actual_line[2]))
        neurone_output = calc_output_neurone(dot, my_neurone)
        if neurone_output != dot.tag:
            nb_errors += 1
            my_neurone = update_neurone(my_neurone, dot, pas_apprentissage)

    print("Nombre final d'erreur : " + format(nb_errors))


if __name__ == '__main__':
    main()
