import numpy as np
import matplotlib.pyplot as plt
import math


# Function f1: (x1-pi)² + (x2+sqrt(2))²
def f1(x):
    x1, x2 = x
    calc_to_return = (x1 - np.pi) ** 2 + (x2 + math.sqrt(2)) ** 2
    return calc_to_return


# Function f5: x1² + x2² - 10 * (cos(2 * pi * x1) + cos(2 * pi * x2))
def f5(x):
    x1, x2 = x

    calc_to_return = x1**2 + x2**2 - 10 * (np.cos(2 * np.pi * x1) + np.cos(2 * np.pi * x2))
    return calc_to_return


def un_plus_un():
    x = 100, 100
    sigma = 0.0005
    gamma = 1.1
    my_list = []
    nb = 10000

    for i in range(0, nb):
        my_list.append(f1(x))
        y = x + sigma * np.random.randn(2)
        if f1(y) < f1(x):
            x = y
            sigma = sigma * gamma
        else:
            sigma = sigma * (gamma ** (-1 / 4))

    plt.semilogy(range(nb), my_list, color='red', label="F1(X) minimisé")
    plt.title("(1+1) - ES")
    plt.grid()
    plt.legend(loc='upper right')
    plt.show()


def mu_lambda():
    nb_iteration = 200
    sigma = 1
    lmbda = math.floor(4 + 3 * np.log(2))
    m_correlation = np.identity(2)
    mu = math.floor(lmbda / 2)
    p_t = 0
    x = [100, 100]
    list_x_f1 = []
    list_sigma = []

    w = np.array([np.log(mu + 0.5) - np.log(i) for i in range(1, mu + 1)])
    w = w / sum(w)

    for i in range(nb_iteration):
        list_x_f1.append(f1(x))
        list_sigma.append(sigma)

        y = x + sigma * np.random.multivariate_normal([0, 0], m_correlation, lmbda)
        y = sorted(y, key=f1)

        tplusun = sum(w[i] * y[i] for i in range(mu))

        tmoinsun = x
        x = tplusun

        # adaptation de sigma
        mu_w = (w[0] + w[1])**2 / sum(w**2)

        z_t = math.sqrt(mu_w) * ((tmoinsun - x) / sigma)

        c_sigma = (2+mu_w) / (2 + 5 + mu_w)

        p_t = (1 - c_sigma) * p_t + math.sqrt(c_sigma * (2 - c_sigma)) * z_t

        new_sigma = sigma * math.exp(c_sigma * (math.sqrt(p_t[0]**2 + p_t[1]**2) / nb_iteration - 1))
        if math.sqrt(p_t[0]**2 + p_t[1]**2) > 2:
            sigma += new_sigma
        else:
            sigma -= new_sigma

    plt.semilogy(range(nb_iteration), list_x_f1, color='red', label="F1(X) minimisé")
    plt.semilogy(range(nb_iteration), list_sigma, color='orange', label="sigma")
    plt.title("(mu,lambda) - ES")
    plt.grid()
    plt.legend(loc='upper right')
    plt.show()


def optim_f1():
    x = [100, 100]
    sigma = 1
    gamma = 1.1
    my_list = []
    list_sigma1 = []
    nb_iteration = 200

    # 1+1-ES
    for i in range(0, nb_iteration):
        my_list.append(f1(x))
        list_sigma1.append(sigma)
        y = x + sigma * np.random.randn(2)
        while y[0] < 0 and y[1] < 0:
            y = x + sigma * np.random.randn(2)
            if f1(y) < f1(x):
                x = y
                sigma = sigma * gamma
            else:
                sigma = sigma * (gamma ** (-1 / 4))

        if f1(y) < f1(x):
            x = y
            sigma = sigma * gamma
        else:
            sigma = sigma * (gamma ** (-1 / 4))



    lmbda = math.floor(4 + 3 * np.log(2))
    m_correlation = np.identity(2)
    mu = math.floor(lmbda / 2)
    p_t = 0
    x = [100, 100]
    sigma = 1
    list_x_f1 = []
    list_sigma = []

    w = np.array([np.log(mu + 0.5) - np.log(i) for i in range(1, mu + 1)])
    w = w / sum(w)

    for i in range(nb_iteration):
        list_x_f1.append(f1(x))
        list_sigma.append(sigma)

        y = x + sigma * np.random.multivariate_normal([0, 0], m_correlation, lmbda)
        y = sorted(y, key=f1)

        tplusun = sum(w[i] * y[i] for i in range(mu))
        while tplusun[0] < 0 and tplusun[1] < 0:
            tplusun = sum(w[i] * y[i] for i in range(mu))
        tmoinsun = x
        x = tplusun

        # adaptation de sigma
        mu_w = (w[0] + w[1]) ** 2 / sum(w ** 2)

        z_t = math.sqrt(mu_w) * ((tmoinsun - x) / sigma)

        c_sigma = (2 + mu_w) / (2 + 5 + mu_w)

        p_t = (1 - c_sigma) * p_t + math.sqrt(c_sigma * (2 - c_sigma)) * z_t

        new_sigma = sigma * math.exp(c_sigma * (math.sqrt(p_t[0] ** 2 + p_t[1] ** 2) / nb_iteration - 1))
        if math.sqrt(p_t[0] ** 2 + p_t[1] ** 2) > 2:
            sigma += new_sigma
        else:
            sigma -= new_sigma

    plt.semilogy(range(nb_iteration), my_list, color='red', label="(1+1) - ES")
    plt.semilogy(range(nb_iteration), list_x_f1, color='orange', label="(mu, lambda) - ES")
    plt.semilogy(range(nb_iteration), list_sigma, color='blue', label="sigma (mu,lambda)")
    plt.semilogy(range(nb_iteration), list_sigma1, color='aqua', label="sigma (1+1)")
    plt.title("F1(X) minimisé")
    plt.grid()
    plt.legend(loc='upper right')
    plt.show()

def main():
    un_plus_un()
    mu_lambda()
    # DERNIERE PARTIE PAS TERMINEE
    #optim_f1()

if __name__ == '__main__':
    main()
