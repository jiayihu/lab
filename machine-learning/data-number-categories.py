import pandas as pd

demo_df = pd.DataFrame({
  'Integer Feature': [0, 1, 2, 1],
  'Categorical Feature': ['socks', 'fox', 'socks', 'box']
})
# print(demo_df)
# print(pd.get_dummies(demo_df))

demo_df['Integer Feature'] = demo_df['Integer Feature'].astype(str)
dummy = pd.get_dummies(demo_df, columns=['Integer Feature', 'Categorical Feature'])
print(dummy.iloc[:, 0:4])
