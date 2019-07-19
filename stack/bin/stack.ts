#!/usr/bin/env node
import 'source-map-support/register';
import cdk = require('@aws-cdk/core');
import { ECStack } from '../lib/ec-stack';

const app = new cdk.App();
new ECStack(app, 'ECStack');
app.synth();