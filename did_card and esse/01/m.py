from json import load,dump
import json,random

with open('cards.json', 'r', encoding="utf-8", errors='ignore') as file:
    data_file = json.load(file)
    category  = data_file['category'][0]
                 
FAQ_MAIN = "Категории - это название для объеденнение тем предмета, области."
FAQ_CATEGORY = ""

inpt = 0
print("\nДля вывода справочника категорий, в консоли необходимо ввести '?'")
print("Для вывода всех категорий, в консоли необходимо ввести ':a'")
print("Для входа в категорию, в консоли необходимо ввести имя категории.")
print("Для выхода из программы, в консоли необходимо ввести ':q'\n")

while inpt != ":q":
    try:
        inpt = str(input(" $: "))

        # Вывод справки Категорий:
        if inpt == '?':
            print(FAQ_MAIN)   

        # Вывод всех категорий:
        if inpt == ':a':
            for curr_category in category:
                print(f'- {curr_category}')
        
        # Добавление новой категории:
        if inpt.startswith("+") and len(inpt.split("+")[-1]) != 0 and len(inpt.split("+")) == 2 and (inpt.split("+")[-1]) not in category: 
            if len(inpt.split("+")[-1]) <= 20:
                category[inpt.split("+")[-1]] = {'cards':{'g1':{},'g2':{},'g3':{},'g4':{},'g5':{}},'doc':{}}
                print(f'Категория {inpt.split("+")[-1]} успешно добавлена!')
            else:
                print('Слишком длинное название категории. Ограничение: 20 символов.')

        # Удаление старой категории:
        if inpt.startswith("-") and len(inpt.split("-")[-1]) != 0 and len(inpt.split("-")) == 2 and (inpt.split("-")[-1]) in category:
            del category[inpt.split("-")[-1]]
            print(f'Категория {inpt.split("-")[-1]} успешно удалена!')    

        # Вход в существующую категорию:
        if inpt in category:
            curr_category = inpt

            print("\nДля вывода справочника, в консоли необходимо ввести '?'")
            print("Для тестирования карточек, в консоли необходимо ввести ':c'")
            print("Для проверки заметок, в консоли необходимо ':d'")
            print("Для выхода в меню категорий, в консоли необходимо ввести ':q'\n")
            # ---------------------------------------------------------------------- #
            while inpt != ':q':
                inpt = str(input(f" {curr_category}$: "))
                      
                # Вывод справки:
                if inpt == '?':
                    print(FAQ_CATEGORY) 

                if inpt == ':d':
                    print('Пока нет')

                # Дедуктивные карточки:
                if inpt == ':c':
                    curr_card_group_num = int(input("Введите группу карт: 1/2/3/4/5"))

                    curr_card_group_key = f'g{curr_card_group_num}'
                    up_card_group_key   = f'g{curr_card_group_num+1}'
                    down_card_group_key = f'g{curr_card_group_num-1}'

                    if up_card_group_key == 'g6':
                        up_card_group_key = 'g5'
                    if down_card_group_key == 'g0':
                        down_card_group_key = 'g1'
                   
                    curr_cards = list(category[curr_category]['cards'][curr_card_group_key].items())
                    random.shuffle(curr_cards)	

                    print("\nНАЧАЛО\n")
                    for key_card, value_card in curr_cards:
                        print("\nСледующее определение:")
                        print(key_card)
                        answ_ = int(input(("\nВыберие 1/2/3 проверить ответ/не знаю/выйти")))

                        if answ_ == 2:
                             del category[curr_category]['cards'][curr_card_group_key][key_card]
                             category[curr_category]['cards'][down_card_group_key][key_card] = value_card
                        elif answ_ == 1:
                            print("ОПРЕДЕЛЕНИЕ:\n")
                            print(value_card)
        
                            answ_2 = int(input(("\nВы ответили правильно 1/2 да/нет")))
        
                            if answ_2 == 2:
                                del category[curr_category]['cards'][curr_card_group_key][key_card]
                                category[curr_category]['cards'][down_card_group_key][key_card] = value_card
                            else:
                                del category[curr_category]['cards'][curr_card_group_key][key_card]
                                category[curr_category]['cards'][up_card_group_key][key_card] = value_card
                        else:
                            break


            # ---------------------------------------------------------------------- #
            inpt = 0
                 


        save_category = {"category":[category]}

        with open('cards.json', 'w', encoding="utf-8", errors='ignore') as file:
            json.dump(save_category, file, indent=2, ensure_ascii=False)


    except:
        print("Что-то пошло не так, вернемся в начало.\n")
