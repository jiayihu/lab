export type RootState = {
  test: number;
};

export const rootReducer = (state: RootState = { test: 1 }) => state;
