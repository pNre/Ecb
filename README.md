## Economic calendar bot

to stay up to date on the events of the financial world

### General info

The bot is build on a serverless architecture using AWS services.

The architecture is composed of 2 Lambdas:

* _Telegram_: webook to receive Telegram updates
* _Calendar_: responsible for fetching the events from the economic calendar (provided by investing.com), persisting them and sending reminders to subscribed users

a pair of DynamoDB tables to hold _subscribers_ and _events_ and a series of CloudWatch Event scheduled to perform events such updating the calendar and pushing upcoming events to subscribers.

All the details of the stack are outlined in `stack/lib/ec-stack.ts`

### Building

#### Requirements

* Docker (some recent version)

#### How to

In `src` just run `build.sh`, it should take care of preparing a Docker container, compiling the source code and packing the binaries for the deployment.

### Deploying

#### Requirements

* Yarn 1.15+

#### How to

The deployment phase is performed via a [CDK](https://github.com/aws/aws-cdk) script. 
Move into the `stack` and run:

```bash
yarn install
yarn build
```

then

```bash
export TELEGRAM=TOKEN_OF_YOUR_TELEGRAM_BOT
cdk deploy
```



