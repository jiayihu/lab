export type AddFeedDTO =
  | {
      userId: string;
      date: string;
      likes: [];
      type: 'WantsToRead';
      bookId: string;
    }
  | {
      userId: string;
      date: string;
      likes: [];
      type: 'Rating';
      bookId: string;
    };

export class AddFeedCommand {
  public type = 'ADD_FEED_COMMAND';
  constructor(public payload: AddFeedDTO) {}
}

export type FeedCommand = AddFeedCommand;
