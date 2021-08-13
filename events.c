#define RLC_VALUE (x) ((x)/10)
#account RLC_ACCOUNT_CREATION_TAX (1)

int wallets[150] = {0};
int coinAddress = 50;
int maxSupply = 1000;

public int buyRLC(int abeySender, int abeyTokens)
{
    if(maxSupply < abeyTokens*10) return 0;
    //OnAfterReceiveTokens( coinAddress, tokens ) = AllocateTokens(abeySender, tokens*10);


    if (OnAfterReceiveTokens( coinAddress, tokens )){

        AllocateTokens(abeySender, abeyTokens*10);
    }
    return 0;
}

public int sellRLC(int abeySender, int abeyTokens)
{
    //OnAfterSendTokens() = ReturnTokens(int WalletIndex, int tokens);

    return 0;
}

public int AllocateTokens(int abeySender, int tokens)
{
    wallets[abeySender] = tokens;
    maxSupply = maxSupply-tokens;
    return 1;
}

public int ReturnTokens(int abeySender, int abeyTokens)
{
    wallets[abeySender] = wallets[abeySender]-abeyTokens;
    maxSupply = maxSupply + abeyTokens;
    return 1;
}

public void createAccount(int abeyAccount)
{
    if OnAfterReceiveTokens( coinAddress, RLC_ACCOUNT_CREATION_TAX )
}

public void printAccount(int abeySender)
{
    printf("Number of tokens in account %d", abeySender);
    printf(" is %d.\n",  wallets[abeySender]);
}