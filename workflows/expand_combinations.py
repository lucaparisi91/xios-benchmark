from typing import List


def expand_combinations(parameters: dict) -> List[dict]:
    """
    Return a list of dictionaries. Each dictionary is a combination of the parameters provided in the parameters dictionary.
    
    :param parameters: Each key is a parameter name and each value is a list of possible values for that parameter. The function will return all possible combinations of these parameters.
    :type parameters: dict
    :return: A list of dictionaries, where each dictionary is a combination of the parameters provided in the parameters dictionary.
    :rtype: List[dict]
    """

    combinations = [{} ]

    for key, values in parameters.items():
        new_combinations = []
        for value in values:
            for combination in combinations:
                new_combination = combination.copy()
                new_combination.update({key: value})
                new_combinations.append(new_combination)
        combinations = new_combinations.copy()
        

    return combinations