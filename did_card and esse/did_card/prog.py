from json import load,dump
import json,random

with open('cards.json', 'r', encoding="utf-8", errors='ignore') as file:
    data_file = json.load(file)
    cards     = data_file['groups'][0]
# -------------------------------------------------------------------- #
curr_card_group_num = int(input("Введите группу карт: 1/2/3"))

curr_card_group_key = f'g{curr_card_group_num}'
up_card_group_key   = f'g{curr_card_group_num+1}'
down_card_group_key = f'g{curr_card_group_num-1}'

if up_card_group_key == 'g4':
    up_card_group_key = 'g3'
if down_card_group_key == 'g0':
    down_card_group_key = 'g1'

# Ключи:
# curr_card_group_key - карточки текущей группы
# up_card_group_key   - карточки группы выше (туда отправлять успешных)
# down_card_group_key - карточки группы ниже (туда отправлять неудачных)
# -------------------------------------------------------------------- #
# Получаем список из пар ключ-значение и перемешиваем их.
curr_cards = list(cards[curr_card_group_key].items())
random.shuffle(curr_cards)
# -------------------------------------------------------------------- #
print("\nНАЧАЛО\n")
for key_card, value_card in curr_cards:
    print("\nСледующее определение:")
    print(key_card)
    answ_ = int(input(("\nВыберие 1/2/3 проверить ответ/не знаю/выйти")))

    if answ_ == 2:
        del cards[curr_card_group_key][key_card]
        cards[down_card_group_key][key_card] = value_card
    elif answ_ == 1:
        print("ОПРЕДЕЛЕНИЕ:\n")
        print(value_card)
        
        answ_2 = int(input(("\nВы ответили правильно 1/2 да/нет")))
        
        if answ_2 == 2:
            del cards[curr_card_group_key][key_card]
            cards[down_card_group_key][key_card] = value_card
        else:
            del cards[curr_card_group_key][key_card]
            cards[up_card_group_key][key_card] = value_card
    else:
        break


# -------------------------------------------------------------------- #
save_cards = {"groups":[cards]}
with open('cards.json', 'w', encoding="utf-8", errors='ignore') as file:
    json.dump(save_cards, file, indent=2, ensure_ascii=False)