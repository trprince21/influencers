from TikTokApi import TikTokApi
import pandas as pd

hashtag = "may"
count = 50000

with TikTokApi() as api:
    tag = api.hashtag(name=hashtag)
    print(tag.info())

    lst = []
    for video in tag.videos(count=count):
        lst.append(video.author.as_dict)

df = pd.DataFrame(lst)
print(df)
