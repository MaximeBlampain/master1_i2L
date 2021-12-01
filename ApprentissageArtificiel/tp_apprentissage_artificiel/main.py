import numpy as np
import math


# ----------- GLOBAL METHODS ----------- #
def init_map(row, column, nb_bottle):
    island = [[0 for i in range(row)] for j in range(column)]
    rhum = nb_bottle
    tresor = 1

    while tresor > 0:
        random_x = np.random.randint(0, column)
        random_y = np.random.randint(0, row)
        if island[random_x][random_y] == 0:
            island[random_x][random_y] = 10
            tresor = tresor - 1

    while rhum > 0:
        random_x = np.random.randint(0, column)
        random_y = np.random.randint(0, row)
        if island[random_x][random_y] == 0:
            island[random_x][random_y] = 2
            rhum = rhum - 1

    return island


def print_map(island):
    for row in island:
        print(row)


def init_pirate(island):
    pirate_position = (0, 0)
    placed = 0
    while placed != 1:
        random_x = np.random.randint(0, len(island[0]))
        random_y = np.random.randint(0, len(island))
        if island[random_x][random_y] == 0:
            pirate_position = (random_x, random_y)
            island[random_x][random_y] = "X"
            placed = 1

    return island, pirate_position


def available_directions(island, pirate):
    map_row, map_column = len(island), len(island[0])
    y, x = pirate
    list_direction = []

    # Mouvement Haut
    if y - 1 >= 0:
        list_direction.append(0)
    # Mouvement Bas
    if y + 1 < map_row:
        list_direction.append(1)
    # Mouvement Droite
    if x + 1 < map_column:
        list_direction.append(2)
    # Mouvement Gauche
    if x - 1 >= 0:
        list_direction.append(3)

    return list_direction


def random_direction(directions):
    random_to_return = np.random.randint(0, len(directions) - 1)
    return random_to_return


def best_move(island, pirate, available_moves):
    y, x = pirate
    final_move = -1
    score_move = -1
    identical_moves = []

    for current_move in available_moves:
        current_score = -1

        # Mouvement Haut
        if current_move == 0:
            current_score = island[y - 1][x]
        # Mouvement Bas
        if current_move == 1:
            current_score = island[y + 1][x]
        # Mouvement Droite
        if current_move == 2:
            current_score = island[y][x + 1]
        # Mouvement Gauche
        if current_move == 3:
            current_score = island[y][x - 1]

        if current_score == score_move:
            identical_moves.append(current_move)
        if current_score > score_move:
            identical_moves = []
            score_move = current_score
            identical_moves.append(current_move)

    if len(identical_moves) > 1:
        final_move = identical_moves[np.random.randint(0, len(identical_moves) - 1)]
    else:
        final_move = identical_moves[0]
    return final_move


def read_move(move):
    # Mouvement Haut
    if move == 0:
        return "Haut"
    # Mouvement Bas
    if move == 1:
        return "Bas"
    # Mouvement Droite
    if move == 2:
        return "Droite"
    # Mouvement Gauche
    if move == 3:
        return "Gauche"

# ----------- EGREEDY METHODS ----------- #

def move(island, pirate, direction):
    score_move = -1
    actual_map = island
    pirate_y, pirate_x = pirate
    pirate_position = (0, 0)
    # Mouvement Haut
    if direction == 0:
        actual_map[pirate_y][pirate_x] = 0
        score_move = actual_map[pirate_y - 1][pirate_x]
        actual_map[pirate_y - 1][pirate_x] = "X"
        pirate_position = pirate_y - 1, pirate_x
    # Mouvement Bas
    if direction == 1:
        actual_map[pirate_y][pirate_x] = 0
        score_move = actual_map[pirate_y + 1][pirate_x]
        actual_map[pirate_y + 1][pirate_x] = "X"
        pirate_position = pirate_y + 1, pirate_x
    # Mouvement Droite
    if direction == 2:
        actual_map[pirate_y][pirate_x] = 0
        score_move = actual_map[pirate_y][pirate_x + 1]
        actual_map[pirate_y][pirate_x + 1] = "X"
        pirate_position = pirate_y, pirate_x + 1
    # Mouvement Gauche
    if direction == 3:
        actual_map[pirate_y][pirate_x] = 0
        score_move = actual_map[pirate_y][pirate_x - 1]
        actual_map[pirate_y][pirate_x - 1] = "X"
        pirate_position = pirate_y, pirate_x - 1

    return actual_map, pirate_position, score_move


def e_greedy(island):
    win = 0
    greedy_map, pirate = init_pirate(island)
    for i in range(0, 20):
        next_move = -1
        move_score = -1
        print("#####################GREEDY MAP#####################")
        print_map(greedy_map)
        list_directions = available_directions(greedy_map, pirate)
        probability = np.random.uniform(0, 1)
        if probability > 0.1:
            print("#####################BEST_MOVE#####################")
            next_move = best_move(greedy_map, pirate, list_directions)
        else:
            print("#####################RANDOM_MOVE#####################")
            next_move = random_direction(list_directions)
        print(read_move(next_move))
        greedy_map, pirate, move_score = move(greedy_map, pirate, next_move)

        if move_score == 10:
            print("Trésor trouvé au bout de ", format(i + 1), " itérations !")
            win = 1
            break

    if win == 0:
        print("Le pirate est reparti sans trésor...")


# ----------- QLEARNING METHODS ----------- #

# QLEARNING Step 2
def calc_matrix(island):
    matrix_to_return = []
    for row in range(len(island)):
        for column in range(len(island[row])):
            position = (row, column)
            directions = available_directions(island, position)
            position_matrix = []
            for i in range(4):
                if i not in directions:
                    position_matrix.append(-1)
                else:
                    if i == 0:
                        position_matrix.append(island[row - 1][column])
                    # Mouvement Bas
                    if i == 1:
                        position_matrix.append(island[row + 1][column])
                    # Mouvement Droite
                    if i == 2:
                        position_matrix.append(island[row][column + 1])
                    # Mouvement Gauche
                    if i == 3:
                        position_matrix.append(island[row][column - 1])
            matrix_to_return.append(position_matrix)

    return matrix_to_return


# QLEARNING Step 3
def move_etat(island, index_etat, action):
    pos_y, pos_x = math.floor(index_etat / len(island[0])), index_etat % len(island[0])

    # Mouvement Haut
    if action == 0:
        pos_y = pos_y - 1
    # Mouvement Bas
    if action == 1:
        pos_y = pos_y + 1
    # Mouvement Droite
    if action == 2:
        pos_x = pos_x + 1
    # Mouvement Gauche
    if action == 3:
        pos_x = pos_x - 1

    result = island[pos_y][pos_x]
    new_etat_index = pos_y * len(island[0]) + pos_x
    position = pos_y, pos_x
    return result, new_etat_index, position


def score_move(island, position, direction):
    pos_y, pos_x = position
    score = -1;
    # Mouvement Haut
    if direction == 0:
        score = island[pos_y - 1][pos_x]
    # Mouvement Bas
    if direction == 1:
        score = island[pos_y + 1][pos_x]
    # Mouvement Droite
    if direction == 2:
        score = island[pos_y][pos_x + 1]
    # Mouvement Gauche
    if direction == 3:
        score = island[pos_y][pos_x - 1]

    return score


# QLEARNING Step 4
class Agent:
    # QLEARNING Step 5
    def calc_q(self, island, etat, action):
        r, new_etat, new_position = move_etat(island, etat, action)
        directions = available_directions(island, new_position)
        best = best_move(island, new_position, directions)
        q = score_move(island, new_position, best)

        calc = r + self.gamma * q
        self.matrix[etat][action] = round(calc)

        return new_etat, r

    # QLEARNING Step 6
    def play_episode(self):
        island = self.island
        score = -1
        etat = np.random.randint(0, len(self.matrix))
        while score != 10:
            position = math.floor(etat / len(island[0])), etat % len(island[0])
            directions = available_directions(island, position)
            r_action = directions[np.random.randint(0, len(directions))]
            etat, score = self.calc_q(island, etat, r_action)

    # QLEARNING Step 7
    def do_n_episode(self, n):
        for i in range(n):
            self.play_episode()

    def get_island_position(self, etat):
        island = self.island
        pos_y, pos_x = math.floor(etat / len(island[0])), etat % len(island[0])
        return pos_y, pos_x

    def get_pirate_etat(self, position):
        pos_y, pos_x = position
        return pos_y * len(self.island[0]) + pos_x

    def get_best_move(self):
        best_mvt_score = -1
        mouvements = self.matrix[self.etat]
        identical_moves = []

        for i in range(len(mouvements)):
            mvt = mouvements[i]
            if mvt == best_mvt_score:
                identical_moves.append(i)
            if mvt > score_move:
                identical_moves = []
                best_mvt_score = mvt
                identical_moves.append(i)

        if len(identical_moves) > 1:
            final_move = identical_moves[np.random.randint(0, len(identical_moves) - 1)]
        else:
            final_move = identical_moves[0]
        return final_move


def init_matrix(island):
    matrix_to_return = [[0 for i in range(4)] for j in range(len(island) * len(island[0]))]
    return matrix_to_return


def print_matrix(matrix):
    for row in matrix:
        print(row)


def qlearning(island):
    win = 0
    print("--------------------- QLEARNING MAP ---------------------")
    print_map(island)

    # QLEARNING Step 4
    agent = Agent()
    agent.island = island
    agent.gamma = 0.9
    agent.matrix = init_matrix(island)

    agent.do_n_episode(50)
    print("MATRIX Q -> ")
    print_matrix(agent.matrix)
    agent.etat = np.random.randint(0, len(agent.matrix))
    """
    last part is not working yet
    for i in range(20):
        next_move = agent.get_best_move()
        agent.island, pirate_position, score_move = move(agent.island, agent.get_island_position(agent.etat),next_move)

        agent.etat = agent.get_pirate_etat(pirate_position)
        if score_move == 10:
            print("Trésor trouvé au bout de ", format(i + 1), " itérations !")
            win = 1
            break

    if win == 0:
        print("Le pirate est reparti sans trésor...")
"""





def main():
    print("--------------------- E GREEDY ---------------------")
    e_greedy_map = init_map(10, 10, 20)
    e_greedy(e_greedy_map)

    print("--------------------- QLEARNING ---------------------")
    qlearning_island = [
        [0, 10],
        [0, 0],
        [2, 0],
        [0, 0],
    ]
    qlearning(qlearning_island)


if __name__ == '__main__':
    main()
