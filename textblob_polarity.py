import pandas as pd
import mysql.connector
from textblob import TextBlob

def polarity_text_blob(x):
    text = TextBlob(x)
    return text.sentiment.polarity

db = mysql.connector.connect(option_files="my.conf", use_pure=True)
with db.cursor() as cursor:
    cursor.execute(
        "SELECT feedback, row_index FROM text_data"
    )
    text_data = cursor.fetchall()
    text_data = pd.DataFrame(text_data)
    text_data.columns = cursor.column_names
    
text_data['polarity'] = text_data['feedback'].apply(polarity_text_blob)
