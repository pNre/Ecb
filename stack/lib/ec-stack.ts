import cdk = require('@aws-cdk/core');
import apigateway = require('@aws-cdk/aws-apigateway');
import lambda = require('@aws-cdk/aws-lambda');
import sqs = require('@aws-cdk/aws-sqs');
import dynamodb = require('@aws-cdk/aws-dynamodb');
import { SqsEventSource } from '@aws-cdk/aws-lambda-event-sources';
import events = require('@aws-cdk/aws-events');
import targets = require('@aws-cdk/aws-events-targets')

if (!("TELEGRAM_BOT_TOKEN" in process.env)) {
  throw new Error("TELEGRAM_BOT_TOKEN env variable not set")
}

const { TELEGRAM_BOT_TOKEN } = process.env

export class ECStack extends cdk.Stack {
  constructor(scope: cdk.Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    const libsslLayer = new lambda.LayerVersion(this, "LibsLayer", {
      code: new lambda.AssetCode('../src/assets/libs.zip')
    });

    const commandsQueue = new sqs.Queue(this, "Commands-Queue", {
      queueName: 'commands'
    });

    const subscribersTable = new dynamodb.Table(this, "Subscribers", {
      tableName: "Subscribers",
      partitionKey: {
        name: "subscription",
        type: dynamodb.AttributeType.STRING
      },
      sortKey: {
        name: "subscriber",
        type: dynamodb.AttributeType.STRING
      }
    });
  
    const eventsTable = new dynamodb.Table(this, "Events", {
      tableName: "Events",
      partitionKey: {
        name: "entity_type",
        type: dynamodb.AttributeType.STRING
      },
      sortKey: {
        name: "event_key",
        type: dynamodb.AttributeType.STRING
      }
    });
  
    const telegramInterface = new lambda.Function(this, "Telegram-Interface", {
      code: new lambda.AssetCode('../src/assets/telegram_interface.zip'),
      handler: 'index.Telegram-Interface',
      runtime: lambda.Runtime.PROVIDED,
      layers: [libsslLayer],
      environment: {
        TELEGRAM_BOT_TOKEN: TELEGRAM_BOT_TOKEN!,
        COMMANDS_QUEUE_URL: commandsQueue.queueUrl
      }
    });
  
    commandsQueue.grantSendMessages(telegramInterface);

    const calendar = new lambda.Function(this, "Calendar", {
      code: new lambda.AssetCode('../src/assets/calendar.zip'),
      handler: 'index.Calendar',
      runtime: lambda.Runtime.PROVIDED,
      layers: [libsslLayer],
      environment: {
        TELEGRAM_BOT_TOKEN: TELEGRAM_BOT_TOKEN!,
        SUBSCRIBERS_TABLE: subscribersTable.tableName,
        EVENTS_TABLE: eventsTable.tableName,
      },
      events: [
        new SqsEventSource(commandsQueue, { batchSize: 1 })
      ]
    });

    subscribersTable.grantReadWriteData(calendar);
    eventsTable.grantReadWriteData(calendar);

    new events.Rule(this, 'UpdateEvents', {
      ruleName: "update-events",
      schedule: events.Schedule.rate(cdk.Duration.minutes(60)),
      targets: [
        new targets.LambdaFunction(calendar, {
          event: events.RuleTargetInput.fromObject({event: 'update-events'})
        })
      ]
    });
  
    new events.Rule(this, 'PushUpdates', {
      ruleName: "push-updates",
      schedule: events.Schedule.rate(cdk.Duration.minutes(2)),
      targets: [
        new targets.LambdaFunction(calendar, {
          event: events.RuleTargetInput.fromObject({event: 'push-updates'})
        })
      ]
    });
  
    const api = new apigateway.RestApi(this, 'EconomicCalendarApi', {
      restApiName: 'Economic Calendar'
    });
    
    const telegram = api.root.addResource('telegram');
    const telegramInterfaceIntegration = new apigateway.LambdaIntegration(telegramInterface);
    telegram.addMethod('POST', telegramInterfaceIntegration);
  }
}

